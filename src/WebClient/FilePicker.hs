{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RebindableSyntax #-}

module FilePicker where

import Prelude hiding ((++), error, putStrLn)
import qualified Prelude as P

import Utils
import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import Knockout
import JQuery (JQuery, select, append)

(++) = T.append
putStrLn = P.putStrLn . T.unpack

getFolderContents :: Text -> (BrowseResponse -> Fay ()) -> Fay ()
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

data FilePickerViewModel = FilePickerViewModel
	{
		displayedFolder :: Observable Text,
		pathElems :: Observable [PathElem],
		files :: ObservableList FileInfo,
		sortedFiles :: ObservableList ClientFileInfo,
		selectedFile :: Observable ClientFileInfo,
		isActive :: FilePickerViewModel -> ClientFileInfo -> Fay Bool,
		selectFile :: FilePickerViewModel -> ClientFileInfo -> Fay (),
		goToFolder :: FilePickerViewModel -> Text -> Fay (),
		okClicked :: ClientFileInfo -> Fay ()
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
	Nothing -> (T.pack $ show v) ++ " bytes"
	where vDouble = fromIntegral v

formatDouble :: Double -> Int -> Text
formatDouble = ffi "%1.toFixed(%2)"

selectFileCb :: FilePickerViewModel -> ClientFileInfo -> Fay ()
selectFileCb filePickerVm fileInfo = if filesize serverInfoV == -1
		then do
			curDisplayedFolder <- koGet $ displayedFolder filePickerVm
			goToFolderCb filePickerVm (curDisplayedFolder ++ "/" ++ filename serverInfoV)
		else do
			fileInfo ~> selectedFile filePickerVm
		where
			serverInfoV = serverInfo fileInfo

goToFolderCb :: FilePickerViewModel -> Text -> Fay ()
goToFolderCb filePickerVm path = do
			path ~> displayedFolder filePickerVm
			refresh filePickerVm

showFilePicker :: Text -> (ClientFileInfo -> Fay ()) -> Fay ()
showFilePicker path callback = do
	holderExists <- select "#filePickerModalHolder" >>= jsLength >>= return . (/= 0)
	when (not holderExists) $ do
		putStrLn "adding the holder"
		select "body" >>= append "<div id='filePickerModalHolder'></div>"
	putStrLn "preparing the modal"
	loadCb "#filePickerModalHolder" "/static/FilePickerModal.html" $ do
		emptyFileList <- koObservableList []
		filepickerRoot <- select "#filePickerModal"
		let filePickerVm = FilePickerViewModel
			{
				displayedFolder = koObservable "",
				pathElems = koObservable [],
				files = emptyFileList,
				sortedFiles = koComputedList $ filesForDisplayCb filePickerVm,
				selectedFile = koObservable $ ClientFileInfo (FileInfo path 0) "",
				isActive = \vm fileInfo -> liftM (fileInfo ==) (koGet $ selectedFile vm),
				selectFile = selectFileCb,
				goToFolder = goToFolderCb,
				okClicked = \x -> callback x >> bootstrapModalHide filepickerRoot
			}
		koApplyBindingsSubTree filePickerVm (first filepickerRoot)
		bootstrapModal filepickerRoot
		refresh filePickerVm

refresh :: FilePickerViewModel -> Fay ()
refresh filePickerVm = do
	displayedFolderV <- koGet $ displayedFolder filePickerVm
	getFolderContents displayedFolderV (readBrowseResponse filePickerVm)

readBrowseResponse :: FilePickerViewModel -> BrowseResponse -> Fay ()
readBrowseResponse filePickerVm browseResponse = do
	browseFolderPath browseResponse ~> displayedFolder filePickerVm
	getPathElements (browseFolderPath browseResponse) ~> pathElems filePickerVm
	koSetList (files filePickerVm) (browseFiles browseResponse)

getPathElements :: Text -> [PathElem]
getPathElements path = (PathElem "root" "/") :
	map (uncurry PathElem) (zip pathElems paths)
	where
		paths = tail $ map ((T.cons '/') . T.intercalate "/") (inits pathElems)
		pathElems = tail $ splitOn "/" path

filesForDisplayCb :: FilePickerViewModel -> Fay [ClientFileInfo]
filesForDisplayCb vm = do
	filesList <- koUnwrapObservableList $ files vm
	let filesListDisplay = filter (\fi -> not $ elem (filename fi) [".", ".."]) filesList
	return $ map convertFileInfo (sortBy filesSort filesListDisplay)

filesSort :: FileInfo -> FileInfo -> Ordering
filesSort a b
	| filesize a == -1 && filesize b >= 0 = LT
	| filesize a >= 0 && filesize b == -1 = GT
	| otherwise = filenameComp a b
		where
			filenameComp = textComp `on` filename
