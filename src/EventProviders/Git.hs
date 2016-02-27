{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, ViewPatterns, RecordWildCards #-}

module Git where

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock (UTCTime(..))
import Text.Parsec.Text
import Text.Parsec
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (isInfixOf, intercalate)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Maybe
import Control.Applicative ( (<$>), (<*>), (<*), (*>) )
import Control.Monad.Trans
import Control.Error
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import TsEvent
import Util
import EventProvider

data GitRecord = GitRecord
    {
        gitUser :: Text,
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

getRepoCommits :: GitRecord -> GlobalSettings -> Day -> (() -> Url) -> ExceptT String IO [TsEvent]
getRepoCommits (GitRecord _username projectPath) _ date _ = do
    let username = T.unpack _username
    output <- Util.runProcess "git" projectPath [
            "log", "--since", showGregorian $ addDays (-1) date,
            "--until", showGregorian $ addDays 1 date,
    --      "--author=\"" ++ username ++ "\"",
            "--stat", "--all", "--decorate"]
    timezone <- liftIO $ getTimeZone (UTCTime date 8)
    allCommits <- hoistEither $ fmapL show $ parse parseCommits "" $ output <> "\n"
    let relevantCommits = filter (isRelevantCommit date username) allCommits
    let addBranch = mapM (getBranch projectPath)
    commitsList <- map (commitToEvent projectPath timezone) <$> addBranch relevantCommits
    let tagCommits = filter (isRelevantTagCommit date) allCommits
    tagsList <- map (tagToEvent projectPath timezone) <$> addBranch tagCommits
    return $ commitsList ++ tagsList

isRelevantCommit :: Day -> String -> Commit -> Bool
isRelevantCommit date username commit = all ($ commit) [
    isInfixOf username . commitAuthor,
    commitInRange date,
    not . commitIsMerge]

isRelevantTagCommit :: Day -> Commit -> Bool
isRelevantTagCommit date commit = all ($ commit) [
    commitInRange date, not . null . commitTags]

getBranch :: FilePath -> Commit -> ExceptT String IO (Commit, Text)
getBranch projectPath commit@Commit{..} = do
    output <- T.lines <$> Util.runProcess "git" projectPath ["branch", "--contains", commitSha]
    return (commit, T.strip $ fromMaybe "" $ headZ output)

commitInRange :: Day -> Commit -> Bool
commitInRange date = inRange . localDay . commitDate
    where
        inRange tdate = tdate >= date && tdate < addDays 1 date

commitToEvent :: FolderPath -> TimeZone -> (Commit, Text) -> TsEvent
commitToEvent gitFolderPath timezone (commit, branch) = TsEvent
    {
        pluginName = getModuleName getGitProvider,
        eventIcon = "glyphicons-423-git-branch",
        eventDate = localTimeToUTC timezone (commitDate commit),
        desc = fromMaybe "no commit message" (commitDesc commit),
        extraInfo = branchDesc <> getCommitExtraInfo commit (T.pack gitFolderPath),
        fullContents = Just $ T.pack $ commitContents commit
    }
    where
      branchDesc = case branch of
          "master"   -> ""
          "* master" -> ""
          _          -> "[" <> branch <> "] "

tagToEvent :: FolderPath -> TimeZone -> (Commit, Text) -> TsEvent
tagToEvent gitFolderPath timezone commit = baseEvent
        {
            desc = "Tag applied: " <> descVal,
            fullContents = Nothing
        }
    where
        baseEvent = commitToEvent gitFolderPath timezone commit
        descVal = T.intercalate ", " $ map T.pack $ commitTags $ fst commit

getCommitExtraInfo :: Commit -> Text -> Text
getCommitExtraInfo commit gitFolderPath = if atRoot filesRoot then gitRepoName else filesRoot
    where
        filesRoot = T.pack $ Util.getFilesRoot $ commitFiles commit
        gitRepoName = last $ T.splitOn "/" gitFolderPath
        atRoot = T.all (`elem` ['.', '/'])

data Commit = Commit
    {
        commitSha      :: String,
        commitDate     :: LocalTime,
        commitDesc     :: Maybe Text,
        commitFiles    :: [String],
        commitAuthor   :: String,
        commitContents :: String,
        commitIsMerge  :: Bool,
        commitTags     :: [String]
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
    sha <- many1 $ noneOf " \n\r"
    tags <- option [] parseDecoration
    many1 $ oneOf "\r\n"

    mergeInfo <- optionMaybe parseMerge
    author <- string "Author: " >> readLine
    date <- parseDateTime
    count 2 eol

    summary <- optionMaybe parseCommitComment
    filesInfo <- optionMaybe parseFiles

    let cFilesDesc = maybe [] (fmap fst) filesInfo
    let cFileNames = maybe [] (fmap snd) filesInfo

    optional (count 2 eol)
    return Commit
        {
            commitSha      = sha,
            commitDate     = date,
            commitDesc     = fmap (T.strip . T.pack) summary,
            commitFiles    = cFileNames,
            commitAuthor   = T.unpack $ T.strip $ T.pack author,
            commitContents = "<pre>" ++ intercalate "<br/>\n" cFilesDesc ++ "</pre>",
            commitIsMerge  = isJust mergeInfo,
            commitTags     = tags
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

parseFilesSummary :: GenParser st ()
parseFilesSummary = do
    char ' '
    many1 digit
    string " file"
    many (noneOf "\n")
    count 2 eol
    return ()

parseDateTime :: GenParser st LocalTime
parseDateTime = do
    string "Date:"
    many (char ' ')
    count 3 anyChar <* char ' ' -- day
    month   <- strToMonth <$> count 3 anyChar
    char ' '
    dayOfMonth <- read <$> many1 (noneOf " ")
    char ' '
    hour    <- Util.parseNum 2 <* char ':'
    mins    <- Util.parseNum 2 <* char ':'
    seconds <- Util.parseNum 2 <* char ' '
    year    <- Util.parseNum 4 <* char ' '
    oneOf "-+" >> count 4 digit
    return $ LocalTime
        (fromGregorian year month dayOfMonth)
        (TimeOfDay hour mins seconds)

monthMap :: Map String Int
monthMap = Map.fromList $ zip ["Jan", "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep", "Oct",
                              "Nov", "Dec"] [1..]

strToMonth :: String -> Int
strToMonth month = fromMaybe (error $ "Unknown month " <> month) $
                   Map.lookup month monthMap

parseCommitComment :: GenParser st String
parseCommitComment = do
    try $ string "    "
    summary <- manyTill anyChar (try $ count 2 eol)
    return summary
