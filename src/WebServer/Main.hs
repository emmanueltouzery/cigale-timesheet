{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase, ViewPatterns, QuasiQuotes #-}
module Main where

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.IO
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Process (rawSystem)
import Control.Applicative
import Network.TCP (openTCPPort)
import Network.Stream (close)
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Error
import System.IO.Error
import qualified Text.Parsec.Text as T
import qualified Text.Parsec as T
import Data.Time
import Control.Monad.Trans
import Control.Monad
import Data.List
import Data.Aeson
import Data.Char (ord)
import Data.Monoid
import System.Environment (getArgs)
import Text.Printf.TH
import GHC.Int (Int64)

import qualified Timesheet
import Config
import Util (parse2, parseNum, parseMaybe, findM)
import Paths_cigale_timesheet
import FilePickerServer (browseFolder)
import SnapUtil (setActionResponse, hParam, noteET)
import qualified EventProviders (plugins)
import EventProvider

appPort :: Int
appPort = 8000

requestBodyReadSize :: Int64
requestBodyReadSize = 65536

appUrl :: String
appUrl = "http://localhost:" ++ show appPort ++ "/cigale"

main :: IO ()
main = do
    args <- getArgs
    getPrefetchFolder >>= createDirectoryIfMissing True
    removeObsoletePrefetch
    if args == ["--prefetch"]
        then doPrefetch
        else startWebApp

doPrefetch :: IO ()
doPrefetch = do
    prefetchFolder <- getPrefetchFolder
    today <- getToday
    -- fetch from the first day of the previous month
    let prefetchStart = addGregorianMonthsClip (-1) $
                        (\(y,m,_) -> fromGregorian y m 1) $ toGregorian today
    -- now fetch from oldestAccepted to yesterday, if needed.
    prefetch prefetchFolder prefetchStart (addDays (-1) today)

getToday :: IO Day
getToday = (localDay . zonedTimeToLocalTime) <$> getZonedTime

removeObsoletePrefetch :: IO ()
removeObsoletePrefetch = do
    -- remove obsolete prefetch files, that means from
    -- let's say 3 months ago.
    oldestAccepted <- addGregorianMonthsClip (-3) <$> getToday
    prefetchFolder <- getPrefetchFolder
    getDirectoryContents prefetchFolder >>= mapM_ (removeIfOlderThan oldestAccepted prefetchFolder)

prefetch :: FilePath -> Day -> Day -> IO ()
prefetch folder curDay maxDay
    | curDay > maxDay = return ()
    | otherwise = do
        unlessM (doesFileExist fname) $ do
            putStrLn $ "prefetching for day " ++ show curDay
            void $ fetchTimesheetAndStore curDay fname
        prefetch folder (addDays 1 curDay) maxDay
    where
        fname = folder </> getPrefetchFilename curDay
        unlessM p r = not <$> p >>= flip when r

getPrefetchFolder :: IO FilePath
getPrefetchFolder = (++ "/prefetch") <$> Config.getSettingsFolder

getPrefetchFilename :: Day -> FilePath
getPrefetchFilename (toGregorian -> (y,m,d)) = [s|%d-%02d-%02d.json|] y m d

removeIfOlderThan :: Day -> FilePath -> FilePath -> IO ()
removeIfOlderThan date folder filename =
    case parseMaybe parsePrefetchFilename (T.pack filename) of
        Nothing       -> return () -- not a prefetch file.
        Just fileDate -> when (fileDate < date) $ removeFile $ folder </> filename

wipePrefetchFiles :: IO ()
wipePrefetchFiles = do
    prefetchFolder <- getPrefetchFolder
    prefetchFiles  <- getDirectoryContents prefetchFolder
    mapM_ (\f -> removeFile (prefetchFolder </> f))
        $ filter (not . isPrefixOf ".") prefetchFiles

parsePrefetchFilename :: T.GenParser st Day
parsePrefetchFilename = parseDate <* T.string ".json"

startWebApp :: IO ()
startWebApp = do
    -- set up the folder where the html/js from the cigale-web
    -- project where copied.
    appDataDir <- Paths_cigale_timesheet.getDataFileName "."
    createDirectoryIfMissing True appDataDir
    let installPath = appDataDir </> "../../cigale-web.jsexe/"
    let snapConfig = setPort appPort .
                     setAccessLog ConfigNoLog .
                     setErrorLog ConfigNoLog $
                     defaultConfig
    forkIO openInBrowser
    httpServe snapConfig (site installPath)

-- wait for the port to be opened then
-- start the browser on the URL of the app.
-- If we start right away the server may
-- not be ready.
openInBrowser :: IO ()
openInBrowser = do
    portOpen <- try (openTCPPort "127.0.0.1" appPort)
    case portOpen of
        Left (_ :: SomeException) -> openInBrowser
        Right conn -> close conn >> openApp

browsers :: [(String, IO ())]
browsers =
    [
        ("google-chrome", void $ rawSystem "google-chrome" ["--app=" ++ appUrl]),
        ("chromium-browser", void $ rawSystem "chromium-browser" ["--app=" ++ appUrl]),
        ("epiphany", runEpiphany)
    ]
fallbackBrowser :: IO ()
fallbackBrowser = void $ rawSystem "xdg-open" [appUrl]

runEpiphany :: IO ()
runEpiphany = do
    putStrLn "WARNING: epiphany as of 3.18 shows some bugs in the rendering."
    settingsFolder <- Config.getSettingsFolder
    let profileDir = settingsFolder ++ "/epiphany-profile-app-cigale-timesheet"
    createDirectoryIfMissing True profileDir
    void $ rawSystem "epiphany" ["--application-mode", "--profile=" ++ profileDir, appUrl]

openApp :: IO ()
openApp = findM (hasProgram . fst) browsers >>= \case
    Nothing       -> fallbackBrowser
    Just (_, cmd) -> cmd

hasProgram :: String -> IO Bool
hasProgram prog = isJust <$> findExecutable prog

site :: FilePath -> Snap ()
site installPath =
    ifTop (redirect "/cigale") <|>
    route [ ("timesheet/:tsparam", timesheet),
            ("configdesc", configdesc),
            ("configVal", configVal),
            ("config", method POST addConfigEntry),
            ("config", method PUT updateConfigEntry),
            ("config", method DELETE deleteConfigEntry),
            ("browseFolder", browseFolder),
            ("getExtraData", httpGetExtraData),
            ("configFetchFieldContents/:providerName", method POST httpConfigFetchFieldContents)
          ] <|>
    dir "cigale" (serveDirectory installPath)

timesheet :: Snap ()
timesheet = setActionResponse $ do
    lift $ do
        modifyResponse $ setContentType "application/json"
        setTimeout 3600
    dateParamText <- TE.decodeUtf8 <$> hParam "tsparam"
    date <- hoistEither $ fmapL BS8.pack $ parse2 parseDate
        "Invalid date format, expected yyyy-mm-dd" dateParamText
    pFname <- (</> getPrefetchFilename date) <$> liftIO getPrefetchFolder
    -- first try to read from prefetch, if it fails, exception, calculate.
    liftIO $ BS.readFile pFname `catch` handleError date pFname
    where handleError date pFname e
              | isDoesNotExistError e = fetchTimesheetAndStore date pFname
              | otherwise             = print e >> throwIO e

fetchTimesheetAndStore :: Day -> FilePath -> IO BS.ByteString
fetchTimesheetAndStore date fname = do
    (success, contents) <- Timesheet.process date
    today <- getToday
    when (not success) $ putStrLn "*** WARNING, errors, will not cache the response"
    -- only cache past dates
    when (date < today && success)
        $ withFile fname WriteMode $ \h -> BSL.hPut h contents
    return $ BSL.toStrict contents

parseDate :: T.GenParser st Day
parseDate = fromGregorian <$> parseNum 4
    <*> (T.char '-' >> parseNum 2) <*> (T.char '-' >> parseNum 2)

configdesc :: Snap ()
configdesc = do
    modifyResponse $ setContentType "application/json"
    writeLBS Timesheet.getEventProvidersConfig

disableCaching :: MonadSnap m => m a -> m a
disableCaching h = do
    modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
    modifyResponse $ setHeader "Pragma" "no-cache"
    h

configVal :: Snap ()
configVal = disableCaching $ do
    settingsFile <- liftIO Config.getConfigFileName
    isSettings   <- liftIO $ doesFileExist settingsFile
    if isSettings
        then serveFile settingsFile
        else writeLBS "[]"

readRequestBodyBS :: Snap BS8.ByteString
readRequestBodyBS = BSL.toStrict <$> readRequestBody requestBodyReadSize

addConfigEntry :: Snap ()
addConfigEntry = setActionResponse $ do
    configItemJson <- lift readRequestBodyBS
    success <- liftIO $ wipePrefetchFiles >> addPluginInConfig configItemJson
    hoistEither success

deleteConfigEntry :: Snap ()
deleteConfigEntry = setActionResponse $ do
    pluginName <- hParam "configItemName"
    liftIO (wipePrefetchFiles >> deletePluginFromConfig pluginName) >>= hoistEither

updateConfigEntry :: Snap ()
updateConfigEntry = setActionResponse $ do
    configItemJson <- lift readRequestBodyBS
    oldConfigItemName <- TE.decodeUtf8 <$> hParam "oldConfigItemName"
    liftIO (wipePrefetchFiles >> updatePluginInConfig oldConfigItemName configItemJson) >>= hoistEither

processConfigFromBody :: (BS.ByteString -> IO (Either BS.ByteString BS.ByteString)) ->
         ExceptT BS.ByteString Snap BS.ByteString
processConfigFromBody handler = do
    pluginName <- hParam "pluginName"
    liftIO (handler pluginName) >>= hoistEither

httpGetExtraData :: Snap ()
httpGetExtraData = setActionResponse $ do
    queryParams  <- hParam "queryParams"
    eventSource  <- httpGetEventSource
    extraData    <- noteET "No extra data" $ getExtraData $ srcProvider eventSource
    decodedParam <- noteET "Error decoding queryParams" $ decodeStrict' queryParams
    settings     <- liftIO Timesheet.getGlobalSettings
    extraDataResult <- liftIO $ extraData (srcConfig eventSource) settings decodedParam
    (contentType, contents) <- noteET "No extra data retrieved" extraDataResult
    lift $ modifyResponse $ setContentType $ BS.pack $ (fromIntegral . ord) <$> contentType
    return contents

httpConfigFetchFieldContents :: Snap ()
httpConfigFetchFieldContents = setActionResponse $ do
    configItemJson <- lift readRequestBodyBS
    lift $ modifyResponse $ setContentType "application/json"
    provName <- TE.decodeUtf8 <$> hParam "providerName"
    settings <- liftIO Timesheet.getGlobalSettings
    provider <- noteET ("Unknown providerName: " <> TE.encodeUtf8 provName) $
        find ((== provName) . T.pack . getModuleName) EventProviders.plugins
    fetchField   <- noteET "No fetch field" $ fetchFieldCts provider
    cfgItemName  <- TE.decodeUtf8 <$> hParam "configItemName"
    cfgItemField <- noteET ("Unknown config item name: " <> TE.encodeUtf8 cfgItemName) $
        find ((== cfgItemName) . T.pack . memberName) (getConfigType provider)
    liftIO $ (BSL.toStrict . encode) <$>
        fetchField cfgItemField (decodeStrict' configItemJson) settings

httpGetEventSource :: ExceptT BS8.ByteString Snap (EventSource Value Value)
httpGetEventSource = do
    cfgItemName  <- TE.decodeUtf8 <$> hParam "configItemName"
    config       <- liftIO $ readConfig EventProviders.plugins
    noteET ("no such config item: " <> TE.encodeUtf8 cfgItemName) $
        find ((==cfgItemName) . srcName) config
