{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, RecursiveDo, RecordWildCards #-}

module FilePicker where

import Reflex.Dom

import System.FilePath.Posix
import Data.List
import Control.Monad.IO.Class
import Data.Monoid
import Data.Char
import Data.Function
import Data.Maybe
import qualified Data.Map as Map

import Common
import Communication

data PathElem = PathElem
    {
        prettyName :: String,
        fullPath :: FilePath
    } deriving Show

data PickerOperationMode = PickFile | PickFolder deriving Eq
data PickerEventType = ChangeFolderEvt FilePath | PickFileEvt FilePath

data FilePickerOptions = FilePickerOptions
    {
        pickerMode :: PickerOperationMode,
        showHiddenFiles :: Bool
    }

pickerDefaultOptions :: FilePickerOptions
pickerDefaultOptions = FilePickerOptions
    {
        pickerMode = PickFile,
        showHiddenFiles = False
    }

isDirectoryFileInfo :: FileInfo -> Bool
isDirectoryFileInfo = (== -1) . filesize

isHiddenFileInfo :: FileInfo -> Bool
isHiddenFileInfo = isPrefixOf "." . filename

getPickData :: PickerEventType -> Maybe FilePath
getPickData (PickFileEvt x) = Just x
getPickData _ = Nothing

getChangeFolder :: PickerEventType -> Maybe FilePath
getChangeFolder (ChangeFolderEvt x) = Just x
getChangeFolder _ = Nothing

buildFilePicker :: MonadWidget t m => FilePickerOptions -> Event t FilePath -> m (Event t FilePath)
buildFilePicker options openEvt = do
    urlAtLoad <- holdDyn Nothing $ Just <$> openEvt
    let noFileEvt = ffilter null openEvt
    let fileEvent = fixFile <$> ffilter (not . null) openEvt
          where fixFile = if pickerMode options == PickFile then dropFileName else id
    rec
        dynBrowseInfo <- sequence
            [
                makeSimpleXhr "/browseFolder" noFileEvt,
                makeSimpleXhr' ("/browseFolder?path=" ++) fileEvent,
                makeSimpleXhr' ("/browseFolder?path=" ++) (fmapMaybe getChangeFolder rx)
            ]
        let browseInfoEvt = leftmost (updated <$> dynBrowseInfo)
        showModalOnEvent ModalLevelSecondary openEvt
        browseDataDyn <- foldDyn const RemoteDataLoading browseInfoEvt
        dynMonPickerEvt <- mapDyn (displayPicker options urlAtLoad) browseDataDyn
        rx <- readModalResult ModalLevelSecondary =<< mapDyn Just dynMonPickerEvt
    let pickedEvent = fmapMaybe getPickData rx
    return pickedEvent

displayPicker :: MonadWidget t m => FilePickerOptions -> Dynamic t (Maybe FilePath) -> RemoteData BrowseResponse
              ->  m (Event t PickerEventType)
displayPicker options urlAtLoad remoteBrowseData = do
    rec
        curSelected <- sample $ current urlAtLoad
        let fetchErrorDyn = constDyn (fromMaybe "" $ remoteDataInvalidDesc remoteBrowseData)
        dynSelectedFile <- holdDyn curSelected
            $ fmap Just
            $ fmapMaybe getPickData pickerEvt
        (pickerEvt, okEvt, _) <- buildModalBody "Pick a folder" (PrimaryBtn "OK") fetchErrorDyn $
            case fromRemoteData remoteBrowseData of
                Nothing -> return never
                Just browseData -> displayPickerContents options dynSelectedFile browseData
        let pickedItemEvt = fmap PickFileEvt $ case pickerMode options of
                PickFolder -> fmap browseFolderPath
                    $ fmapMaybe (const $ fromRemoteData remoteBrowseData) okEvt
                PickFile -> fmapMaybe id $ tagDyn dynSelectedFile okEvt
        hideModalOnEvent ModalLevelSecondary pickedItemEvt
    return $ leftmost [pickerEvt, pickedItemEvt]

displayPickerContents :: MonadWidget t m => FilePickerOptions
                      -> Dynamic t (Maybe FilePath) -> BrowseResponse
                      -> m (Event t PickerEventType)
displayPickerContents options dynSelectedFile browseData = do
    let path = browseFolderPath browseData
    breadcrumbR <- elAttr "ol" ("class" =: "breadcrumb") $ do
        let pathLevels = reverse $ foldl' formatPathLinks [] (splitPath path)
        leftmost <$> displayBreadcrumb pathLevels
    rec
        fileInfoEvent <- displayFiles browseData dynSelectedFile dynPickerOptions
        dynShowHidden <- fmap _checkbox_value $ el "label" $
            checkbox (showHiddenFiles options) def <* text "Show hidden files"
        dynPickerOptions <- forDyn dynShowHidden $ \sh ->
            FilePickerOptions { showHiddenFiles = sh, pickerMode = pickerMode options }
    let readTableEvent fi = let fullPath = path </> filename fi in
            if isDirectoryFileInfo fi
            then ChangeFolderEvt fullPath
            else PickFileEvt fullPath
    return $ leftmost [fmap ChangeFolderEvt breadcrumbR, fmap readTableEvent fileInfoEvent]

displayBreadcrumb :: MonadWidget t m => [PathElem] -> m [Event t FilePath]
displayBreadcrumb [] = return []
displayBreadcrumb [level] = do
    (li, _) <- elAttr' "li" ("class" =: "active") $ text (prettyName level)
    return [fmap (const "/") $ domEvent Click li]
displayBreadcrumb (level:xs) = do
    (lnk, _) <- el "li" $ elAttr' "a" ("href" =: "javascript:void(0)") $ text (prettyName level)
    (:) <$> return (fmap (const $ fullPath level) $ domEvent Click lnk) <*> displayBreadcrumb xs

displayFiles :: MonadWidget t m => BrowseResponse -> Dynamic t (Maybe FilePath)
             -> Dynamic t FilePickerOptions -> m (Event t FileInfo)
displayFiles browseData dynSelectedFile dynPickerOptions =
    elAttr "div" ("style" =: ("overflow-y: auto; overflow-x: hidden"
                              <> "min-height: 370px; max-height: 370px; width: 100%")) $
        elAttr "table" ("class" =: "table table-sm") $ do
            dynEvt <- forDyn dynPickerOptions $ \pickerOptions ->
                leftmost <$> mapM
                    (displayFile dynSelectedFile)
                    (getFiles browseData pickerOptions)
            readDynMonadicEvent dynEvt

getFiles :: BrowseResponse -> FilePickerOptions -> [FileInfo]
getFiles browseData FilePickerOptions{..} =
    browseFiles browseData
            & sortBy filesSort
            & filter (\fi -> filename fi `notElem` [".", ".."])
            & filter (\fi -> showHiddenFiles || not (isHiddenFileInfo fi))
            & filter (\fi -> pickerMode == PickFile || isDirectoryFileInfo fi)

filesSort :: FileInfo -> FileInfo -> Ordering
filesSort a b
    | isDirectoryFileInfo a && not (isDirectoryFileInfo b) = LT
    | not (isDirectoryFileInfo a) && isDirectoryFileInfo b = GT
    | otherwise = filenameComp a b
  where
    filenameComp = compare `on` (fmap toLower . filename)

displayFile :: MonadWidget t m => Dynamic t (Maybe FilePath) -> FileInfo
            -> m (Event t FileInfo)
displayFile dynSelectedFile file@FileInfo{..} = do
    dynRowAttr <- forDyn dynSelectedFile $ \selFile ->
        if fmap takeFileName selFile == Just filename
            then ("class" =: "table-active") else Map.empty
    (row, _) <- elDynAttr' "tr" dynRowAttr $ do
        elAttr "td" ("align" =: "center"
                     <> "style" =: "width: 30px") $ rawPointerSpan $
            constDyn (if isDirectoryFileInfo file then "&#x1f5c1;" else "&#x1f5ce;")
        elAttr "td" ("style" =: "cursor: pointer") $ text filename
    return $ fmap (const file) (domEvent Click row)

formatPathLinks :: [PathElem] -> String -> [PathElem]
formatPathLinks [] _ = [PathElem "root" "/"]
formatPathLinks l@(previous:_) n =  PathElem (withoutSlashes n) (fullPath previous </> n):l
    where withoutSlashes = filter (/= '/')
