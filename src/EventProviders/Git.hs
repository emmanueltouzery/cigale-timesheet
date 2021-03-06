{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell, ViewPatterns, DataKinds #-}

module Git where

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock (UTCTime(..))
import Text.Parsec.Text
import Text.Parsec
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (isInfixOf, intercalate)
import Data.Aeson.TH
import Data.Maybe
import Control.Monad.Trans
import Control.Error
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import TsEvent
import Util
import EventProvider
import EventProviderSettings

deriveConfigRecord gitConfigDataType
deriveJSON defaultOptions ''GitConfigRecord

getGitProvider :: EventProvider GitConfigRecord ()
getGitProvider = EventProvider
    {
        getModuleName = "Git",
        getEvents     = getRepoCommits,
        getConfigType = members gitConfigDataType,
        getExtraData  = Nothing,
        fetchFieldCts = Just gitFetchField
    }

gitFetchField :: ConfigDataInfo -> Maybe GitConfigRecord -> GlobalSettings
              -> ExceptT String IO [Text]
gitFetchField _ Nothing _ = return []
gitFetchField _ (Just gitRecord) _ = do
    output <- Util.runProcess "git" (gitRepo gitRecord) ["shortlog", "-sn", "HEAD"]
    hoistEither $ fmapL show $ parse parseGitUserList "" output

parseGitUserList :: GenParser st [Text]
parseGitUserList = many parseGitUser

parseGitUser :: GenParser st Text
parseGitUser = do
    many (oneOf "\t ")
    many1 digit
    char '\t'
    username <- manyTill anyChar (try eol)
    return (T.pack username)

getRepoCommits :: GitConfigRecord -> GlobalSettings -> Day -> (() -> Url)
               -> ExceptT String IO [TsEvent]
getRepoCommits (GitConfigRecord projectPath _username) _ date _ = do
    let username = T.unpack _username
    output <- Util.runProcess "git" projectPath [
            "log", "--since", showGregorian $ addDays (-1) date,
            "--until", showGregorian $ addDays 1 date,
    --      "--author=\"" ++ username ++ "\"",
            "--stat", "--all", "--decorate", "--pretty=fuller"]
    timezone <- liftIO $ getTimeZone (UTCTime date 8)
    allCommits <- hoistEither $ fmapL show $ parse parseCommits "" $ output <> "\n"
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

commitToEvent :: FolderPath -> TimeZone -> Commit -> TsEvent
commitToEvent gitFolderPath timezone commit = TsEvent
    {
        pluginName = getModuleName getGitProvider,
        eventIcon = if commitIsCherryPicked commit
                       then "glyphicons-428-git-pull-request"
                       else "glyphicons-423-git-branch",
        eventDate = localTimeToUTC timezone (commitDate commit),
        desc = fromMaybe "no commit message" (commitDesc commit),
        extraInfo = getCommitExtraInfo commit (T.pack gitFolderPath),
        fullContents = Just $ T.pack $ commitContents commit
    }

tagToEvent :: FolderPath -> TimeZone -> Commit -> TsEvent
tagToEvent gitFolderPath timezone commit = baseEvent
        {
            desc = "Tag applied: " <> descVal,
            fullContents = Nothing
        }
    where
        baseEvent = commitToEvent gitFolderPath timezone commit
        descVal = T.intercalate ", " $ map T.pack $ commitTags commit

getCommitExtraInfo :: Commit -> Text -> Text
getCommitExtraInfo commit gitFolderPath = if atRoot filesRoot then gitRepoName else filesRoot
    where
        filesRoot = T.pack $ Util.getFilesRoot $ commitFiles commit
        gitRepoName = last $ T.splitOn "/" gitFolderPath
        atRoot = T.all (`elem` ['.', '/'])

data Commit = Commit
    {
        commitDate     :: LocalTime,
        commitDesc     :: Maybe Text,
        commitFiles    :: [String],
        commitAuthor   :: String,
        commitContents :: String,
        commitIsMerge  :: Bool,
        commitTags     :: [String],
        commitIsCherryPicked :: Bool
    }
    deriving (Eq, Show)

parseCommits :: GenParser st [Commit]
parseCommits = many parseCommit

parseMerge :: GenParser st String
parseMerge = string "Merge: " *> readLine

parseDecoration :: GenParser st [String]
parseDecoration = do
    string " ("
    decorationItems <- (try parseTag <|> parseParent) `sepBy` string ", "
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
    many1 $ noneOf " \n\r" -- commit sha
    tags <- option [] parseDecoration
    many1 $ oneOf "\r\n"

    mergeInfo <- optionMaybe parseMerge
    author <- string "Author: " >> stripLine
    authorDate <- string "AuthorDate:" *> parseDateTime <* eol
    string "Commit: " >> stripLine
    cDate <- string "CommitDate:" *> parseDateTime <* eol
    eol

    summary <- optionMaybe parseCommitComment
    filesInfo <- optionMaybe parseFiles

    let cFilesDesc = maybe [] (fmap fst) filesInfo
    let cFileNames = maybe [] (fmap snd) filesInfo

    optional (count 2 eol)
    let isCherryPicked = cDate /= authorDate
    let cherryPickInfo = if isCherryPicked
          then "<table><tr><td><b>Original author</b></td><td>"
               ++ author ++ "</td></tr>"
               ++ "<tr><td><b>Original commit</b></td><td>"
               ++ show authorDate ++ "</td></tr></table>"
          else ""
    return Commit
        {
            commitDate     = cDate,
            commitDesc     = fmap (T.strip . T.pack) summary,
            commitFiles    = cFileNames,
            commitAuthor   = author,
            commitContents = cherryPickInfo ++ "<pre>" ++ intercalate "<br/>\n" cFilesDesc ++ "</pre>",
            commitIsMerge  = isJust mergeInfo,
            commitTags     = tags,
            commitIsCherryPicked = isCherryPicked
        }

stripLine :: GenParser st String
stripLine = T.unpack . T.strip . T.pack <$> readLine

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
    manyTill anyChar (try $ count 2 eol)
