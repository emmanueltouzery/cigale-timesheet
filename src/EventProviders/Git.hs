{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, ViewPatterns #-}

module Git where

import qualified System.Process as Process
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock (UTCTime(..))
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.List (isInfixOf, intercalate, foldl')
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Maybe
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )

import Event
import qualified Util
import EventProvider

data GitRecord = GitRecord
	{
		gitUser :: T.Text,
		gitRepo :: FolderPath
	} deriving Show
deriveJSON defaultOptions ''GitRecord

getGitProvider :: EventProvider GitRecord
getGitProvider = EventProvider
	{
		getModuleName = "Git",
		getEvents = getRepoCommits,
		getConfigType = members $(thGetTypeDesc ''GitRecord)
	}

getRepoCommits :: GitRecord -> GlobalSettings -> Day -> IO [Event.Event]
getRepoCommits (GitRecord _username projectPath) _ date = do
	let username = T.unpack _username
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "git" [
			"log", "--since", formatDate $ addDays (-1) date,
			"--until", formatDate $ addDays 1 date,
	--		"--author=\"" ++ username ++ "\"",
			"--stat", "--all", "--decorate"])
		{
			Process.std_out = Process.CreatePipe,
			Process.cwd = Just projectPath
		}
	output <- IO.hGetContents outh
	timezone <- getTimeZone (UTCTime date 8)
	let allCommits = Util.parsecError parseCommits "Git.getRepoCommits" $ T.concat [output, "\n"]
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
			commitInRange date,
			not . null . commitTags]

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

parseCommits :: T.GenParser st [Commit]
parseCommits = many parseCommit --manyTill parseCommit (T.try eof)

parseMerge :: T.GenParser st String
parseMerge = string "Merge: " *> readLine

parseDecoration :: T.GenParser st [String]
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

parseTag :: T.GenParser st DecorationItem
parseTag = Tag <$> (string "tag: " *> many1 (T.noneOf ",)"))

parseParent :: T.GenParser st DecorationItem
parseParent = Parent <$> many1 (T.noneOf ",)")

parseCommit :: T.GenParser st Commit
parseCommit = do
	string "commit "
	commitSha <- many1 $ T.noneOf " \n\r"
	tags <- option [] parseDecoration
	many1 $ T.oneOf "\r\n"
	
	mergeInfo <- optionMaybe parseMerge
	string "Author: "
	author <- readLine
	date <- parseDateTime
	eol
	eol

	summary <- optionMaybe parseCommitComment
	filesInfo <- optionMaybe parseFiles

	let cFilesDesc = maybe [] (fmap fst) filesInfo
	let cFileNames = maybe [] (fmap snd) filesInfo

	optional eol
	optional eol
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

readLine :: T.GenParser st String
readLine = (T.many $ T.noneOf "\r\n") <* T.oneOf "\r\n"

parseFiles :: T.GenParser st [(String, String)]
parseFiles = manyTill parseFile (T.try parseFilesSummary)

parseFile :: T.GenParser st (String, String)
parseFile = do
	char ' '
	result <- T.many $ T.noneOf "|"
	rest <- T.many $ T.noneOf "\n"
	eol
	return (result ++ rest, T.unpack $ T.strip $ T.pack result)

parseFilesSummary :: T.GenParser st String
parseFilesSummary = do
	char ' '
	many1 digit
	string " file"
	T.many $ T.noneOf "\n"
	eol

parseDateTime :: T.GenParser st LocalTime
parseDateTime = do
	string "Date:"
	many $ T.char ' '
	count 3 T.anyChar <* T.char ' ' -- day
	month <- strToMonth <$> count 3 T.anyChar
	T.char ' '
	dayOfMonth <- read <$> (T.many1 $ T.noneOf " ")
	T.char ' '
	hour <- Util.parseNum 2 <* T.char ':'
	mins <- Util.parseNum 2 <* T.char ':'
	seconds <- Util.parseNum 2 <* T.char ' '
	year <- Util.parseNum 4 <* T.char ' '
	oneOf "-+"
	count 4 digit
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

parseCommitComment :: T.GenParser st String
parseCommitComment = do
	T.try $ string "    "
	summary <- manyTill anyChar (T.try $ string "\n\n" <|> string "\r\n\r\n")
	count 2 eol
	return summary

eol :: T.GenParser st String
eol = T.many $ T.oneOf "\r\n"
