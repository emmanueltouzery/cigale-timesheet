{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RebindableSyntax #-}

module FilePicker (getFolderContents, showFilePicker,
	OperationMode(..), FilePickerOptions(..), pickerDefaultOptions) where

import Prelude hiding ((++), error, putStrLn, last)
import qualified Prelude as P

import Utils
import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import Knockout
import JQuery (JQuery, select, append)

(++) = T.append
putStrLn = P.putStrLn . T.unpack

getFolderContents :: Text -> (Automatic BrowseResponse -> Fay ()) -> Fay ()
getFolderContents path callback = ajxGet url (putStrLn "Error getting folder contents") callback
	where url = case path of
		"" -> "/browseFolder"
		_ -> "/browseFolder?path=" ++ path

data BrowseResponse = BrowseResponse
	{
		browseFolderPath :: Text,
		browseFiles :: [FileInfo]
	}

data FileInfo = FileInfo
	{
		filename :: Text,
		-- filesize will be -1 for a directory
		filesize :: Int
	} deriving Eq

data ClientFileInfo = ClientFileInfo
	{
		serverInfo :: FileInfo,
		filesizeDesc :: Text
	} deriving Eq

data OperationMode = PickFile | PickFolder deriving Eq

data FilePickerOptions = FilePickerOptions
	{
		pickerMode :: OperationMode,
		showHiddenFiles :: Bool
	}
pickerDefaultOptions = FilePickerOptions
	{
		pickerMode = PickFile,
		showHiddenFiles = False
	}

data FilePickerViewModel = FilePickerViewModel
	{
		operationMode :: OperationMode,
		optShowHiddenFiles :: Observable Bool,
		operationModeStr :: Observable Text,
		displayedFolder :: Observable Text,
		pathElems :: Observable [PathElem],
		files :: ObservableList FileInfo,
		sortedFiles :: ObservableList ClientFileInfo,
		selectedFile :: Observable ClientFileInfo,
		isActive :: FilePickerViewModel -> ClientFileInfo -> Fay Bool,
		selectFile :: FilePickerViewModel -> ClientFileInfo -> Fay (),
		goToFolder :: FilePickerViewModel -> Text -> Fay (),
		okClicked :: Fay ()
	}
instance KnockoutModel FilePickerViewModel

data PathElem = PathElem
	{
		pathElemName :: Text,
		fullPath :: Text
	}

convertFileInfo :: FileInfo -> ClientFileInfo
convertFileInfo serverInfoV = ClientFileInfo
	{
		serverInfo = serverInfoV,
		filesizeDesc = getSizeDesc $ filesize serverInfoV
	}

sizeUnits :: [(Double, Text)]
sizeUnits = reverse $ zip (map (1024**) [1..]) ["kb", "Mb", "Gb"]

getSizeDesc :: Int -> Text
getSizeDesc v | v == -1 = "-"
getSizeDesc v | v == -2 = ""
getSizeDesc v = case find (\s -> vDouble >= fst s) sizeUnits of
	Just (mult, desc) -> formatDouble (vDouble / mult) 2 ++ desc
	Nothing -> T.pack (show v) ++ " bytes"
	where vDouble = fromIntegral v

formatDouble :: Double -> Int -> Text
formatDouble = ffi "%1.toFixed(%2)"

selectFileCb :: FilePickerViewModel -> ClientFileInfo -> Fay ()
selectFileCb filePickerVm fileInfo = if filesize serverInfoV == -1
		then do
			curDisplayedFolder <- koGet $ displayedFolder filePickerVm
			goToFolderCb filePickerVm
				(ensureEndSlash curDisplayedFolder ++ filename serverInfoV)
		else fileInfo ~> selectedFile filePickerVm
		where
			serverInfoV = serverInfo fileInfo

goToFolderCb :: FilePickerViewModel -> Text -> Fay ()
goToFolderCb filePickerVm path = do
			path ~> displayedFolder filePickerVm
			refresh filePickerVm

showFilePicker :: Text -> FilePickerOptions -> (Text -> Fay ()) -> Fay ()
showFilePicker path options callback = do
	let opMode = pickerMode options
	holderExists <- liftM (/=0) (select "#filePickerModalHolder" >>= jsLength)
	unless holderExists $ select "body" >>= append "<div id='filePickerModalHolder'></div>"
	loadCb "#filePickerModalHolder" "/static/FilePickerModal.html" $ do
		emptyFileList <- koObservableList []
		filepickerRoot <- select "#filePickerModal"
		let (curFolder, curFile) = case path of
			"" -> ("", "")
			_ -> case opMode of
				PickFile -> breakOnEnd "/" path
				PickFolder -> (path, "")
		let filePickerVm = FilePickerViewModel
			{
				operationMode = opMode,
				optShowHiddenFiles = koObservable $ showHiddenFiles options,
				operationModeStr = koComputed $ return $ if opMode == PickFile
					then "PickFile" else "PickFolder",
				displayedFolder = koObservable curFolder,
				pathElems = koObservable [],
				files = emptyFileList,
				sortedFiles = koComputedList $ filesForDisplayCb filePickerVm,
				selectedFile = koObservable $ ClientFileInfo (FileInfo curFile 0) "",
				isActive = isActiveCb,
				selectFile = selectFileCb,
				goToFolder = goToFolderCb,
				okClicked = okClickedCb filePickerVm callback filepickerRoot
			}
		koGet (optShowHiddenFiles filePickerVm) >>= print
		koApplyBindingsSubTree filePickerVm (first filepickerRoot)
		bootstrapModal filepickerRoot
		koSubscribe (optShowHiddenFiles filePickerVm) (\_ -> refresh filePickerVm)
		refresh filePickerVm

isActiveCb :: FilePickerViewModel -> ClientFileInfo -> Fay Bool
isActiveCb vm fileInfo = do
	selectedFileName <- koGet $ selectedFile vm
	return $ fEquals fileInfo selectedFileName
	where fEquals = (==) `on` (filename . serverInfo)

refresh :: FilePickerViewModel -> Fay ()
refresh filePickerVm = do
	displayedFolderV <- koGet $ displayedFolder filePickerVm
	getFolderContents displayedFolderV (readBrowseResponse filePickerVm)

okClickedCb :: FilePickerViewModel -> (Text -> Fay ()) -> JQuery -> Fay ()
okClickedCb vm callback filepickerRoot = do
	folder <- koGet $ displayedFolder vm
	file <- koGet $ selectedFile vm
	let folderSlash = ensureEndSlash folder
	case operationMode vm of
		PickFile -> callback $ folderSlash ++ filename (serverInfo file)
		PickFolder -> callback folderSlash
	bootstrapModalHide filepickerRoot

ensureEndSlash :: Text -> Text
ensureEndSlash path = case last path of
	'/' -> path
	_ -> path ++ "/"

readBrowseResponse :: FilePickerViewModel -> Automatic BrowseResponse -> Fay ()
readBrowseResponse filePickerVm browseResponse = do
	browseFolderPath browseResponse ~> displayedFolder filePickerVm
	getPathElements (browseFolderPath browseResponse) ~> pathElems filePickerVm
	let allFiles = browseFiles browseResponse
	let filesToDisplay = case operationMode filePickerVm of
		PickFile -> allFiles
		PickFolder -> filter ((==(-1)) . filesize) allFiles
	showHiddenFiles <- koGet $ optShowHiddenFiles filePickerVm
	let finalList = if showHiddenFiles
		then filesToDisplay
		else filter ((/='.') . T.head . filename) filesToDisplay
	koSetList (files filePickerVm) finalList

getPathElements :: Text -> [PathElem]
getPathElements path = PathElem "root" "/" : zipWith PathElem pathElems paths
	where
		paths = tail $ map (T.cons '/' . T.intercalate "/") (inits pathElems)
		pathElems = tail $ splitOn "/" path

filesForDisplayCb :: FilePickerViewModel -> Fay [ClientFileInfo]
filesForDisplayCb vm = do
	filesList <- koUnwrapObservableList $ files vm
	let filesListDisplay = filter (\fi -> notElem (filename fi) [".", ".."]) filesList
	return $ map convertFileInfo (sortBy filesSort filesListDisplay)

filesSort :: FileInfo -> FileInfo -> Ordering
filesSort a b
	| filesize a == -1 && filesize b >= 0 = LT
	| filesize a >= 0 && filesize b == -1 = GT
	| otherwise = filenameComp a b
		where
			filenameComp = textComp `on` (toLower . filename)
