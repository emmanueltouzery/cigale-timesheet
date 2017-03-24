{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo, RecordWildCards #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, LambdaCase, TemplateHaskell #-}

module FilePicker where

import Reflex.Dom hiding (display)

import System.FilePath.Posix
import Data.List
import Data.Char
import Data.Function
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Clay hiding (filter, (&), id, reverse, li, a, b)
import Control.Lens
import Control.Lens.TH

import Common
import Communication

data PathElem = PathElem
    {
        prettyName :: Text,
        fullPath   :: FilePath
    } deriving Show

data PickerOperationMode = PickFile | PickFolder deriving Eq
data PickerEventType = ChangeFolderEvt FilePath | PickFileEvt FilePath deriving Show
$(makePrisms ''PickerEventType)

data FilePickerOptions = FilePickerOptions
    {
        pickerMode      :: PickerOperationMode,
        showHiddenFiles :: Bool
    }

pickerDefaultOptions :: FilePickerOptions
pickerDefaultOptions = FilePickerOptions
    {
        pickerMode      = PickFile,
        showHiddenFiles = False
    }

isDirectoryFileInfo :: FileInfo -> Bool
isDirectoryFileInfo = (== -1) . filesize

isHiddenFileInfo :: FileInfo -> Bool
isHiddenFileInfo = isPrefixOf "." . filename

buildFilePicker :: MonadWidget t m => FilePickerOptions -> Event t FilePath -> m (Event t FilePath)
buildFilePicker options openEvt = do
    urlAtLoad <- holdDyn Nothing $ Just <$> openEvt
    let noFileEvt = ffilter null openEvt
    let fileEvent = fixFile <$> ffilter (not . null) openEvt
          where fixFile = T.pack . if pickerMode options == PickFile then dropFileName else id
    rec
        dynBrowseInfo <- sequence
            [
                makeSimpleXhr "/browseFolder" noFileEvt,
                makeSimpleXhr' ("/browseFolder?path=" <>) fileEvent,
                makeSimpleXhr' ("/browseFolder?path=" <>) (fmapMaybe (fmap T.pack . preview _ChangeFolderEvt) rx)
            ]
        let browseInfoEvt = leftmost (updated <$> dynBrowseInfo)
        browseDataDyn <- foldDyn const RemoteDataLoading browseInfoEvt
        rx <- displayPicker options urlAtLoad browseDataDyn openEvt
    return (fmapMaybe (preview _PickFileEvt) rx)

displayPicker :: MonadWidget t m => FilePickerOptions -> Dynamic t (Maybe FilePath)
              -> Dynamic t (RemoteData BrowseResponse) -> Event t FilePath
              -> m (Event t PickerEventType)
displayPicker options urlAtLoad remoteBrowseDataDyn openEvt = do
    rec
        curSelected <- sample $ current urlAtLoad
        let fetchErrorDyn = fromMaybe "" <$> preview _RemoteDataInvalid <$> remoteBrowseDataDyn
        dynSelectedFile <- holdDyn curSelected
            $ fmap Just
            $ fmapMaybe (preview _PickFileEvt) pickerEvt
        let contentsDyn = ffor (fromRemoteData <$> remoteBrowseDataDyn) $ \case
                Nothing         -> return never
                Just browseData -> displayPickerContents options dynSelectedFile browseData
        displayDyn <- toggle True $
            leftmost [const () <$> openEvt, const () <$> pickedItemEvt
                     , dlgCloseEvt pickerDlg]
        let divStyle v = ("style"::Text) =: styleStr (styleWithHideIf v $
                position fixed >> left (px 0) >> right (px 0) >>
                top (px 0) >> bottom (px 0) >> zIndex 15000)
        pickerDlg <- elDynAttr "div" (divStyle <$> displayDyn) $
            buildModalBody' "Pick a folder" (PrimaryBtn "OK") fetchErrorDyn contentsDyn
        let pickerEvt = switchPromptlyDyn (dlgContentsDyn pickerDlg)
        let pickedItemEvt = fmap PickFileEvt $ case pickerMode options of
                PickFolder -> fmap browseFolderPath
                    $ fmapMaybe fromRemoteData $ (tagPromptlyDyn remoteBrowseDataDyn) (dlgOkEvt pickerDlg)
                PickFile -> fmapMaybe id $ tagPromptlyDyn dynSelectedFile $ dlgOkEvt pickerDlg
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
        let dynPickerOptions = ffor dynShowHidden $ \sh ->
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
    return [const "/" <$> domEvent Click li]
displayBreadcrumb (level:xs) = do
    (lnk, _) <- el "li" $ elAttr' "a" ("href" =: "javascript:void(0)") $ text (prettyName level)
    (:) <$> return (const (fullPath level) <$> domEvent Click lnk) <*> displayBreadcrumb xs

displayFiles :: MonadWidget t m => BrowseResponse -> Dynamic t (Maybe FilePath)
             -> Dynamic t FilePickerOptions -> m (Event t FileInfo)
displayFiles browseData dynSelectedFile dynPickerOptions = do
    let divStyle = do
            overflowY auto
            overflowX hidden
            width (pct 100)
            minHeight (px 150)
            maxHeight (px 150)
    elStyle "div" divStyle $
        elAttr "table" ("class" =: "table table-sm") $ do
            let dynEvt = ffor dynPickerOptions $ \pickerOptions ->
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
    let dynRowAttr = ffor dynSelectedFile $ \selFile ->
          if fmap takeFileName selFile == Just filename
          then ("class" =: "table-active") else Map.empty
    (rowItem, _) <- elDynAttr' "tr" dynRowAttr $ do
        elAttrStyle "td" ("align" =: "center") (width $ px 30) $ rawPointerSpan $
            constDyn (if isDirectoryFileInfo file then "&#x1f5c1;" else "&#x1f5ce;")
        elStyle "td" (cursor pointer) $ text $ T.pack filename
    return $ fmap (const file) (domEvent Click rowItem)

formatPathLinks :: [PathElem] -> String -> [PathElem]
formatPathLinks [] _ = [PathElem "root" "/"]
formatPathLinks l@(previous:_) n =  PathElem (withoutSlashes n) (fullPath previous </> n):l
    where withoutSlashes = T.pack . filter (/= '/')
