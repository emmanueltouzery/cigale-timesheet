{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, RecursiveDo, RecordWildCards #-}

module FilePicker where

import Reflex.Dom

import Data.Aeson as A
import GHC.Generics
import System.FilePath.Posix
import Data.List
import Control.Monad.IO.Class
import Data.Monoid
import Data.Char
import Data.Function

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

-- TODO display error if the XHR fails (eg server is down)
buildFolderPicker :: MonadWidget t m => Event t () -> m (Event t FilePath)
buildFolderPicker clickEvt = do
    rec
        (dynUpdateEvents :: [Dynamic t (RemoteData BrowseResponse)]) <- sequence
            [
                makeSimpleXhr "/browseFolder" clickEvt,
                makeSimpleXhr' ("/browseFolder?path=" ++) (fmapMaybe getChangeFolder rx)
            ]
        browseDataDyn <- foldDyn const RemoteDataLoading $ leftmost (updated <$> dynUpdateEvents)
        (bla :: Dynamic t (m (Event t PickerEventType))) <- forDyn browseDataDyn $ \remoteBrowseData -> do
            let modalId = topLevelModalId ModalLevelSecondary
            performEvent_ $ fmap (const $ liftIO $ showModalIdDialog modalId) clickEvt
            (r :: Event t PickerEventType, okEvt, _) <- buildModalBody "Pick a folder" (PrimaryBtn "OK") clickEvt never $ do
                case fromRemoteData remoteBrowseData of
                    Nothing -> return never
                    Just browseData -> do
                        breadcrumbR <- elAttr "ol" ("class" =: "breadcrumb") $ do
                            let path = browseFolderPath browseData
                            let pathLevels = reverse $ foldl' formatPathLinks [] (splitPath path)
                            leftmost <$> displayBreadcrumb pathLevels
                        tableR <- displayFiles browseData
                        return $ leftmost [fmap ChangeFolder breadcrumbR, fmap (PickFileFolder . filename) tableR] -- ## filename is not ok, need the full path
            return r
        rx <- readModalResult ModalLevelSecondary bla
        --(rDynEvent :: Dynamic t (Event t PickerEventType)) <- holdDyn never (bodyResult r)
        --let rx = switch $ current (bodyResult r)
        --let x = fmapMaybe getPickData rx
    --return x
    return never

displayBreadcrumb :: MonadWidget t m => [PathElem] -> m [Event t FilePath]
displayBreadcrumb [] = return []
displayBreadcrumb [level] = do
    (li, _) <- elAttr' "li" ("class" =: "active") $ text (prettyName level)
    return [fmap (const "/") $ domEvent Click li]
displayBreadcrumb (level:xs) = do
    (lnk, _) <- el "li" $ elAttr' "a" ("href" =: "javascript:void(0)") $ text (prettyName level)
    (:) <$> return (fmap (const $ fullPath level) $ domEvent Click lnk) <*> displayBreadcrumb xs

displayFiles :: MonadWidget t m => BrowseResponse -> m (Event t FileInfo)
displayFiles browseData = do
    let files = browseFiles browseData
            & sortBy filesSort
            & filter (\fi -> filename fi `notElem` [".", ".."])
    elAttr "div" ("style" =: ("overflow-y: auto; overflow-x: hidden"
                              <> "min-height: 370px; max-height: 370px; width: 100%")) $
        elAttr "table" ("class" =: "table table-sm") $
            leftmost <$> mapM displayFile files

-- directories first then alphabetical sorting
filesSort :: FileInfo -> FileInfo -> Ordering
filesSort a b
    | filesize a == -1 && filesize b >= 0 = LT
    | filesize a >= 0 && filesize b == -1 = GT
    | otherwise = filenameComp a b
  where
    filenameComp = compare `on` (fmap toLower . filename)

displayFile :: MonadWidget t m => FileInfo -> m (Event t FileInfo)
displayFile file@FileInfo{..} = do
    (row, _) <- el' "tr" $ el "td" (text filename)
    return $ fmap (const file) (domEvent Click row)

formatPathLinks :: [PathElem] -> String -> [PathElem]
formatPathLinks [] _ = [PathElem "root" "/"]
formatPathLinks l@(previous:_) n =  PathElem (withoutSlashes n) (fullPath previous </> n):l
    where withoutSlashes = filter (/= '/')
