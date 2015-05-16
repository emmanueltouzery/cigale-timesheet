{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, ViewPatterns #-}

module Git where

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock (UTCTime(..))
import Text.Parsec.Text
import Text.Parsec
import qualified Data.Text as T
import Data.List (isInfixOf, intercalate)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Maybe
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Monad (replicateM_)
import Control.Monad.Trans
import Control.Error

import Event
import qualified Util
import EventProvider

data GitRecord = GitRecord
	{
		gitUser :: T.Text,
		gitRepo :: FolderPath
	} deriving Show
deriveJSON defaultOptions ''GitRecord

getGitProvider :: EventProvider GitRecord ()
getGitProvider = EventProvider
	{
		getModuleName = "Git",
		getEvents = getRepoCommits,
		getConfigType = members $(thGetTypeDesc ''GitRecord),
		getExtraData = Nothing
	}

getRepoCommits :: GitRecord -> GlobalSettings -> Day -> (() -> Url) -> EitherT String IO [Event.Event]
getRepoCommits (GitRecord _username projectPath) _ date _ = do
	let username = T.unpack _username
	output <- Util.runProcess "git" projectPath [
			"log", "--since", formatDate $ addDays (-1) date,
			"--until", formatDate $ addDays 1 date,
	--		"--author=\"" ++ username ++ "\"",
			"--stat", "--all", "--decorate"]
	timezone <- liftIO $ getTimeZone (UTCTime date 8)
	allCommits <- hoistEither $ fmapL show $ parse parseCommits "" $ T.concat [output, "\n"]
	let relevantCommits = filter (isRelevantCommit date username) allCommits
	let commitsList = map (commitToEvent projectPath timezone) relevantCommits
	let tagCommits = filter (isRelevantTagCommit date) allCommits
	let tagsList = map (tagToEvent projectPath timezone) tagCommits
	return $ commitsList ++ tagsList

isRelevantCommit :: Day -> String -> Commit -> Bool
isRelevantCommit date username commit = all ($ commit) [
	isInfixOf username . commitAuthor,
	commitInRange date,
	not . commitIsMerge]

isRelevantTagCommit :: Day -> Commit -> Bool
isRelevantTagCommit date commit = all ($ commit) [
	commitInRange date, not . null . commitTags]

commitInRange :: Day -> Commit -> Bool
commitInRange date = inRange . localDay . commitDate
	where
		inRange tdate = tdate >= date && tdate < addDays 1 date

commitToEvent :: FolderPath -> TimeZone -> Commit -> Event.Event
commitToEvent gitFolderPath timezone commit = Event.Event
	{
		pluginName = getModuleName getGitProvider,
		eventIcon = "glyphicon-cog",
		eventDate = localTimeToUTC timezone (commitDate commit),
		desc = fromMaybe "no commit message" (commitDesc commit),
		extraInfo = getCommitExtraInfo commit (T.pack gitFolderPath),
		fullContents = Just $ T.pack $ commitContents commit
	}

tagToEvent :: FolderPath -> TimeZone -> Commit -> Event.Event
tagToEvent gitFolderPath timezone commit = baseEvent
		{
			desc = T.concat ["Tag applied: ", descVal],
			fullContents = Nothing
		}
	where
		baseEvent = commitToEvent gitFolderPath timezone commit
		descVal = T.intercalate ", " $ map T.pack $ commitTags commit

getCommitExtraInfo :: Commit -> T.Text -> T.Text
getCommitExtraInfo commit gitFolderPath = if atRoot filesRoot then gitRepoName else filesRoot
	where
		filesRoot = T.pack $ Util.getFilesRoot $ commitFiles commit
		gitRepoName = last $ T.splitOn "/" gitFolderPath
		atRoot = T.all (`elem` "./")

formatDate :: Day -> String
formatDate (toGregorian -> (year, month, dayOfMonth)) =
	show year ++ "-" ++ show month ++ "-" ++ show dayOfMonth

data Commit = Commit
	{
		commitDate :: LocalTime,
		commitDesc :: Maybe T.Text,
		commitFiles :: [String],
		commitAuthor :: String,
		commitContents :: String,
		commitIsMerge :: Bool,
		commitTags :: [String]
	}
	deriving (Eq, Show)

parseCommits :: GenParser st [Commit]
parseCommits = many parseCommit

parseMerge :: GenParser st String
parseMerge = string "Merge: " *> readLine

parseDecoration :: GenParser st [String]
parseDecoration = do
	string " ("
	decorationItems <- many1 $ do
		val <- parseTag <|> parseParent
		optional $ string ", "
		return val
	string ")"
	return $ map (\(Tag a) -> a) $ filter isTag decorationItems

data DecorationItem = Parent String
	| Tag String
	deriving Show

isTag :: DecorationItem -> Bool
isTag (Tag _) = True
isTag _ = False

parseTag :: GenParser st DecorationItem
parseTag = Tag <$> (string "tag: " *> many1 (noneOf ",)"))

parseParent :: GenParser st DecorationItem
parseParent = Parent <$> many1 (noneOf ",)")

parseCommit :: GenParser st Commit
parseCommit = do
	string "commit "
	commitSha <- many1 $ noneOf " \n\r"
	tags <- option [] parseDecoration
	many1 $ oneOf "\r\n"

	mergeInfo <- optionMaybe parseMerge
	author <- string "Author: " >> readLine
	date <- parseDateTime
	eol >> eol

	summary <- optionMaybe parseCommitComment
	filesInfo <- optionMaybe parseFiles

	let cFilesDesc = maybe [] (fmap fst) filesInfo
	let cFileNames = maybe [] (fmap snd) filesInfo

	replicateM_ 2 $ optional eol
	return Commit
		{
			commitDate = date,
			commitDesc = fmap (T.strip . T.pack) summary,
			commitFiles = cFileNames,
			commitAuthor = T.unpack $ T.strip $ T.pack author,
			commitContents = "<pre>" ++ intercalate "<br/>\n" cFilesDesc ++ "</pre>",
			commitIsMerge = isJust mergeInfo,
			commitTags = tags
		}

readLine :: GenParser st String
readLine = many (noneOf "\r\n") <* oneOf "\r\n"

parseFiles :: GenParser st [(String, String)]
parseFiles = manyTill parseFile (try parseFilesSummary)

parseFile :: GenParser st (String, String)
parseFile = do
	char ' '
	result <- many $ noneOf "|"
	rest <- many $ noneOf "\n"
	eol
	return (result ++ rest, T.unpack $ T.strip $ T.pack result)

parseFilesSummary :: GenParser st String
parseFilesSummary = do
	char ' '
	many1 digit
	string " file"
	many $ noneOf "\n"
	eol

parseDateTime :: GenParser st LocalTime
parseDateTime = do
	string "Date:"
	many $ char ' '
	count 3 anyChar <* char ' ' -- day
	month <- strToMonth <$> count 3 anyChar
	char ' '
	dayOfMonth <- read <$> many1 (noneOf " ")
	char ' '
	hour <- Util.parseNum 2 <* char ':'
	mins <- Util.parseNum 2 <* char ':'
	seconds <- Util.parseNum 2 <* char ' '
	year <- Util.parseNum 4 <* char ' '
	oneOf "-+" >> count 4 digit
	return $ LocalTime
		(fromGregorian year month dayOfMonth)
		(TimeOfDay hour mins seconds)

strToMonth :: String -> Int
strToMonth month = case month of
	"Jan" -> 1
	"Feb" -> 2
	"Mar" -> 3
	"Apr" -> 4
	"May" -> 5
	"Jun" -> 6
	"Jul" -> 7
	"Aug" -> 8
	"Sep" -> 9
	"Oct" -> 10
	"Nov" -> 11
	"Dec" -> 12
	_ -> error $ "Unknown month " ++ month

parseCommitComment :: GenParser st String
parseCommitComment = do
	try $ string "    "
	summary <- manyTill anyChar (try $ string "\n\n" <|> string "\r\n\r\n")
	count 2 eol
	return summary

eol :: GenParser st String
eol = many $ oneOf "\r\n"
