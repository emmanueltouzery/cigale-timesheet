{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

module Git where

import qualified System.Process as Process
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.List (isInfixOf, intercalate)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Maybe

import Event as Event
import qualified Util
import EventProvider

data GitRecord = GitRecord
	{
		gitUser :: T.Text,
		gitRepo :: T.Text
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
getRepoCommits (GitRecord _username _projectPath) _ date = do
	let username = T.unpack _username
	let projectPath = T.unpack _projectPath
	(inh, Just outh, errh, pid) <- Process.createProcess
		(Process.proc "git" [
			"log", "--since", formatDate $ addDays (-1) date,
			"--until", formatDate $ addDays 1 date,
	--		"--author=\"" ++ username ++ "\"",
			"--stat", "--all"])
		{
			Process.std_out = Process.CreatePipe,
			Process.cwd = Just projectPath
		}
	output <- IO.hGetContents outh
	timezone <- getCurrentTimeZone
	let parseResult = parseCommitsParsec $ T.concat [output, "\n"]
	case parseResult of
		Left pe -> do
			print $ T.unpack output
			putStrLn $ "GIT: parse error: " ++ Util.displayErrors pe
			error "GIT parse error, aborting"
			--return []
		Right x -> do
			let myCommits = filter ((isInfixOf username) . commitAuthor) x
			let myCommitsInInterval = filter (inRange . localDay . commitDate) myCommits
			let myNonMergeCommitsInInterval = filter (not . commitIsMerge) myCommitsInInterval
			return $ map (toEvent _projectPath timezone) myNonMergeCommitsInInterval
	where
		inRange tdate = (tdate >= date && tdate < (addDays 1 date))
	
toEvent :: T.Text -> TimeZone -> Commit -> Event.Event
toEvent gitFolderPath timezone commit =
	Event.Event
		{
			pluginName = getModuleName getGitProvider,
			eventDate = (localTimeToUTC timezone (commitDate commit)),
			desc = case commitDesc commit of
			  	Nothing -> "no commit message"
			  	Just x -> x,
			extraInfo = getCommitExtraInfo commit gitFolderPath,
			fullContents = Just $ T.pack $ commitContents commit
		}

getCommitExtraInfo :: Commit -> T.Text -> T.Text
getCommitExtraInfo commit gitFolderPath = if atRoot filesRoot then gitRepoName else filesRoot
	where
		filesRoot = T.pack $ Util.getFilesRoot $ commitFiles commit
		gitRepoName = last $ T.splitOn "/" gitFolderPath
		atRoot = T.all (`elem` "./")

formatDate :: Day -> String
formatDate day =
	(show year) ++ "-" ++ (show month) ++ "-" ++ (show dayOfMonth)
	where
		(year, month, dayOfMonth) = toGregorian day

parseCommitsParsec :: T.Text -> Either ParseError [Commit]
parseCommitsParsec = parse parseCommits ""

data Commit = Commit
	{
		commitDate :: LocalTime,
		commitDesc :: Maybe T.Text,
		commitFiles :: [String],
		commitAuthor :: String,
		commitContents :: String,
		commitIsMerge :: Bool
	}
	deriving (Eq, Show)

parseCommits :: T.GenParser st [Commit]
parseCommits = many parseCommit --manyTill parseCommit (T.try eof)

parseMerge :: T.GenParser st String
parseMerge = do
	string "Merge: "
	readLine

parseCommit :: T.GenParser st Commit
parseCommit = do
	string "commit "
	commit <- readLine
	mergeInfo <- optionMaybe parseMerge
	string "Author: "
	author <- readLine
	date <- parseDateTime
	eol
	eol

	--sections <- many parseSection
	_sections0 <- optionMaybe parseSection
	_sections1 <- if isNothing mergeInfo
		then optionMaybe parseSection
		else return Nothing

	-- TODO this filter not null is ugly.. the parseSection
	-- should just return Nothing :-(
	let sections = fmap fromJust $ filter isJust [_sections0, _sections1]

	--traceShow sections (optional eol)

	-- these sections can be either comment or
	-- list of files that were changed by the commit.
	-- if this is a merge, there will not be files
	-- and i expect only one section to exist
	let (summary, filesText) = if isJust mergeInfo
			then (Util.maybeHead sections, Nothing)
			else case length sections of
				2 -> (Just $ head sections, Just $ sections !! 1)
				1 ->	-- is that section comment or files??
					if isFiles $ head sections
						then (Nothing, Just $ head sections)
						else (Just $ head sections, Nothing)
				0 -> (Nothing, Nothing)
			     -- if there is one in sections need to find
			     -- out, is that files or comment
			     -- no sections then no summary

	--traceShow (summary, filesText) (optional eol)

	(cFileNames, cFilesDesc) <- case filesText of
		Just filesContents -> do
			case parse parseFiles "" (T.pack $ fromJust filesText) of
				Right cFiles -> return (fmap snd cFiles, fmap fst cFiles)
				Left pe -> do
					error $ "GIT file list parse error, aborting; commit is " ++ commit ++ " " ++ Util.displayErrors pe
					return ([],[])
				
		Nothing -> return ([], [])
	optional eol
	optional eol
	return $ Commit
		{
			commitDate = date,
			commitDesc = fmap (T.strip . T.pack) summary,
			commitFiles = cFileNames,
			commitAuthor = T.unpack $ T.strip $ T.pack author,
			commitContents = "<pre>" ++ intercalate "<br/>\n" cFilesDesc ++ "</pre>",
			commitIsMerge = isJust mergeInfo
		}
	where
		isFiles = not . null . filter (== '\n')

readLine :: T.GenParser st String
readLine = do
	result <- T.many $ T.noneOf "\r\n"
	T.oneOf "\r\n"
	return result

parseFiles :: T.GenParser st [(String, String)]
parseFiles = manyTill parseFile (T.try parseFilesSummary)

parseFile :: T.GenParser st (String, String)
parseFile = do
	optional $ char ' ' -- optional because it won't occur on the first line.
			    -- that's because we eat it to make sure
			    -- it's not already the next commit. On
			    -- subsequent lines we don't eat it.
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
	count 3 T.anyChar -- day
	T.char ' '
	month <- count 3 T.anyChar
	T.char ' '
	dayOfMonth <- T.many1 $ T.noneOf " "
	T.char ' '
	hour <- count 2 digit
	T.char ':'
	mins <- count 2 digit
	T.char ':'
	seconds <- count 2 digit
	T.char ' '
	year <- count 4 digit
	T.char ' '
	oneOf "-+"
	count 4 digit
	return $ LocalTime
		(fromGregorian (Util.parsedToInteger year) (strToMonth month) (Util.parsedToInt dayOfMonth))
		(TimeOfDay (Util.parsedToInt hour) (Util.parsedToInt mins) (fromIntegral $ Util.parsedToInt seconds))

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

parseSection :: T.GenParser st String
parseSection = do
	string " " -- the sections are indented by one character.
		   -- I need this so i don't think the beginning of the next
		   -- commit is a section of the current commit.
	summary <- manyTill anyChar (T.try $ string "\n\n" <|> string "\r\n\r\n")
	count 2 eol
	return summary

eol :: T.GenParser st String
eol = T.many $ T.oneOf "\r\n"
