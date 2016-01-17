{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, RecursiveDo, RecordWildCards #-}

module FilePicker where

import Reflex.Dom

import Data.Aeson as A
import GHC.Generics
import System.FilePath.Posix
import Data.List

import Common

-- TODO stop copying from the server!!
data FileInfo = FileInfo
    {
        filename :: String,
        -- filesize will be -1 for a directory
        filesize :: Integer
    } deriving (Show, Generic)
instance FromJSON FileInfo

data BrowseResponse = BrowseResponse
    {
        browseFolderPath :: String,
        browseFiles :: [FileInfo]
    } deriving (Show, Generic)
instance FromJSON BrowseResponse

data PathElem = PathElem
    {
        prettyName :: String,
        fullPath :: FilePath
    } deriving Show

data PickerEventType = ChangeFolder FilePath | PickFileFolder FilePath

getPickData :: PickerEventType -> Maybe FilePath
getPickData (PickFileFolder x) = Just x
getPickData _ = Nothing

getChangeFolder :: PickerEventType -> Maybe FilePath
getChangeFolder (ChangeFolder x) = Just x
getChangeFolder _ = Nothing

buildFolderPicker :: MonadWidget t m => Event t () -> m (Event t FilePath)
buildFolderPicker clickEvt = do
    rec
        (dynUpdateEvents :: [Dynamic t (RemoteData BrowseResponse)]) <- sequence
            [
                makeSimpleXhr "/browseFolder" clickEvt,
                makeSimpleXhr' ("/browseFolder?path=" ++) (fmapMaybe getChangeFolder rx)
            ]
        browseDataDyn <- foldDyn const RemoteDataLoading $ leftmost (updated <$> dynUpdateEvents)
        -- and tie it to the event for appearance.
        (r :: ModalDialogResult t (Event t PickerEventType)) <- buildModalDialog "Pick a folder" (PrimaryBtn "OK") clickEvt never $ do
            bla <- forDyn browseDataDyn $ \remoteBrowseData -> do
                case fromRemoteData remoteBrowseData of
                    Nothing -> return never
                    Just browseData -> do
                        breadcrumbR <- elAttr "ol" ("class" =: "breadcrumb") $ do
                            --breadcrumb <- do
                            let path = browseFolderPath browseData
                            let pathLevels = reverse $ foldl' formatPathLinks [] (splitPath path)
                            leftmost <$> displayBreadcrumb pathLevels
                            -- TODO that's messy.. ask on irc or something,
                            -- there's probably a nicer way?
                            -- bDynEvent <- dyn breadcrumb >>= holdDyn never
                            -- return $ fmap ChangeFolder (switch $ current bDynEvent)
                        tableR <- el "table" $ do
                            let files = browseFiles browseData
                            leftmost <$> mapM displayFile files
                        return $ leftmost [fmap ChangeFolder breadcrumbR, fmap (PickFileFolder . filename) tableR] -- ## filename is not ok, need the full path
            -- TODO that's messy.. ask on irc or something,
            -- there's probably a nicer way?
            (eer :: Event t (Event t PickerEventType)) <- dyn bla
            (eet :: Dynamic t (Event t PickerEventType)) <- holdDyn never eer
            return $ switch $ current eet
        --(rDynEvent :: Dynamic t (Event t PickerEventType)) <- holdDyn never (bodyResult r)
        let rx = switch $ current (bodyResult r)
        let x = fmapMaybe getPickData rx
    return x

displayFile :: MonadWidget t m => FileInfo -> m (Event t FileInfo)
displayFile file@FileInfo{..} = do
    (row, _) <- el' "tr" $ el "td" (text filename)
    return $ fmap (const file) (domEvent Click row)

formatPathLinks :: [PathElem] -> String -> [PathElem]
formatPathLinks [] _ = [PathElem "root" "/"]
formatPathLinks l@(previous:_) n =  PathElem (withoutSlashes n) (fullPath previous </> n):l
    where withoutSlashes = filter (/= '/')

displayBreadcrumb :: MonadWidget t m => [PathElem] -> m [Event t FilePath]
displayBreadcrumb [] = return []
displayBreadcrumb [level] = do
    (li, _) <- elAttr' "li" ("class" =: "active") $ text (prettyName level)
    return [fmap (const "/") $ domEvent Click li]
displayBreadcrumb (level:xs) = do
    (lnk, _) <- el "li" $ elAttr' "a" ("href" =: "javascript:void(0)") $ text (prettyName level)
    (:) <$> return (fmap (const $ fullPath level) $ domEvent Click lnk) <*> displayBreadcrumb xs

-- todo get me a [(String, FilePath)] so i can interpret the click to a full path.
prettyPrintPathLevel :: String -> String
prettyPrintPathLevel level = if null withoutSlashes then "root" else withoutSlashes
    where withoutSlashes = filter (/= '/') level
