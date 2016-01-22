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
import Data.Maybe

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

isDirectoryFileInfo :: FileInfo -> Bool
isDirectoryFileInfo = (== -1) . filesize

isHiddenFileInfo :: FileInfo -> Bool
isHiddenFileInfo = isPrefixOf "." . filename

getPickData :: PickerEventType -> Maybe FilePath
getPickData (PickFileFolder x) = Just x
getPickData _ = Nothing

getChangeFolder :: PickerEventType -> Maybe FilePath
getChangeFolder (ChangeFolder x) = Just x
getChangeFolder _ = Nothing

buildFolderPicker :: MonadWidget t m => Event t FilePath  -> m (Event t FilePath)
buildFolderPicker openEvt = do
    let noFileEvt = ffilter null openEvt
    let fileEvent = ffilter (not . null) openEvt
    rec
        dynBrowseInfo <- sequence
            [
                makeSimpleXhr "/browseFolder" noFileEvt,
                makeSimpleXhr' ("/browseFolder?path=" ++) fileEvent,
                makeSimpleXhr' ("/browseFolder?path=" ++) (fmapMaybe getChangeFolder rx)
            ]
        let browseInfoEvt = leftmost (updated <$> dynBrowseInfo)
        browseDataDyn <- foldDyn const RemoteDataLoading browseInfoEvt
        fetchErrorDyn <- mapDyn (fromMaybe "" . remoteDataInvalidDesc) browseDataDyn
        dynMonPickerEvt <- forDyn browseDataDyn (\remoteBrowseData -> do
            let modalId = topLevelModalId ModalLevelSecondary
            performEvent_ $ fmap (const $ liftIO $ showModalIdDialog modalId) openEvt
            (r, okEvt, _) <- buildModalBody "Pick a folder" (PrimaryBtn "OK") fetchErrorDyn $
                case fromRemoteData remoteBrowseData of
                    Nothing -> return never
                    Just browseData -> displayPickerContents browseData
            let pickedFolderEvt = fmap (PickFileFolder . browseFolderPath)
                    $ fmapMaybe (const $ fromRemoteData remoteBrowseData) okEvt
            return $ leftmost [r, pickedFolderEvt])
        rx <- readModalResult ModalLevelSecondary dynMonPickerEvt
    return $ fmapMaybe getPickData rx

displayPickerContents :: MonadWidget t m => BrowseResponse -> m (Event t PickerEventType)
displayPickerContents browseData = do
    let path = browseFolderPath browseData
    breadcrumbR <- elAttr "ol" ("class" =: "breadcrumb") $ do
        let pathLevels = reverse $ foldl' formatPathLinks [] (splitPath path)
        leftmost <$> displayBreadcrumb pathLevels
    rec
        tableR <- displayFiles browseData dynShowHidden
        dynShowHidden <- fmap _checkbox_value $ el "label" $
            checkbox False def <* text "Show hidden files"
    let readTableEvent fi = let fullPath = path </> filename fi in
            if isDirectoryFileInfo fi
            then ChangeFolder fullPath
            else PickFileFolder fullPath
    return $ leftmost [fmap ChangeFolder breadcrumbR, fmap readTableEvent tableR]

displayBreadcrumb :: MonadWidget t m => [PathElem] -> m [Event t FilePath]
displayBreadcrumb [] = return []
displayBreadcrumb [level] = do
    (li, _) <- elAttr' "li" ("class" =: "active") $ text (prettyName level)
    return [fmap (const "/") $ domEvent Click li]
displayBreadcrumb (level:xs) = do
    (lnk, _) <- el "li" $ elAttr' "a" ("href" =: "javascript:void(0)") $ text (prettyName level)
    (:) <$> return (fmap (const $ fullPath level) $ domEvent Click lnk) <*> displayBreadcrumb xs

displayFiles :: MonadWidget t m => BrowseResponse -> Dynamic t Bool -> m (Event t FileInfo)
displayFiles browseData dynShowHidden =
    elAttr "div" ("style" =: ("overflow-y: auto; overflow-x: hidden"
                              <> "min-height: 370px; max-height: 370px; width: 100%")) $
        elAttr "table" ("class" =: "table table-sm") $ do
            dynEvt <- forDyn dynShowHidden $ \showHidden ->
                leftmost <$> mapM displayFile (getFiles browseData showHidden)
            readDynMonadicEvent dynEvt

getFiles :: BrowseResponse -> Bool -> [FileInfo]
getFiles browseData showHidden =
    browseFiles browseData
            & sortBy filesSort
            & filter (\fi -> filename fi `notElem` [".", ".."])
            & filter (\fi -> showHidden || not (isHiddenFileInfo fi))
            & filter isDirectoryFileInfo

filesSort :: FileInfo -> FileInfo -> Ordering
filesSort a b
    | isDirectoryFileInfo a && not (isDirectoryFileInfo b) = LT
    | not (isDirectoryFileInfo a) && isDirectoryFileInfo b = GT
    | otherwise = filenameComp a b
  where
    filenameComp = compare `on` (fmap toLower . filename)

displayFile :: MonadWidget t m => FileInfo -> m (Event t FileInfo)
displayFile file@FileInfo{..} = do
    (row, _) <- el' "tr" $ do
        elAttr "td" ("align" =: "center"
                     <> "style" =: "width: 30px") $ rawPointerSpan $
            constDyn (if isDirectoryFileInfo file then "&#x1f5c1;" else "&#x1f5ce;")
        elAttr "td" ("style" =: "cursor: pointer") $ text filename
    return $ fmap (const file) (domEvent Click row)

formatPathLinks :: [PathElem] -> String -> [PathElem]
formatPathLinks [] _ = [PathElem "root" "/"]
formatPathLinks l@(previous:_) n =  PathElem (withoutSlashes n) (fullPath previous </> n):l
    where withoutSlashes = filter (/= '/')
