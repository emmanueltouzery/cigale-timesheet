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
	}

data FilePickerViewModel = FilePickerViewModel
	{
		displayedFolder :: Observable Text,
		files :: ObservableList FileInfo,
		sortedFiles :: ObservableList FileInfo,
		selectedFile :: Observable FileInfo,
		selectFile :: FilePickerViewModel -> FileInfo -> Fay (),
		okClicked :: FileInfo -> Fay ()
	}
instance KnockoutModel FilePickerViewModel

selectFileCb :: FilePickerViewModel -> FileInfo -> Fay ()
selectFileCb filePickerVm fileInfo = if filesize fileInfo == -1
		then do
			curDisplayedFolder <- koGet $ displayedFolder filePickerVm
			(curDisplayedFolder ++ "/" ++ filename fileInfo) ~> displayedFolder filePickerVm
			refresh filePickerVm
		else do
			fileInfo ~> selectedFile filePickerVm
			print fileInfo

showFilePicker :: Text -> (FileInfo -> Fay ()) -> Fay ()
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
				files = emptyFileList,
				sortedFiles = koComputedList $ filesForDisplayCb filePickerVm,
				selectedFile = koObservable $ FileInfo path 0,
				okClicked = \x -> callback x >> bootstrapModalHide filepickerRoot,
				selectFile = selectFileCb
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
	koSetList (files filePickerVm) (browseFiles browseResponse)

filesForDisplayCb :: FilePickerViewModel -> Fay [FileInfo]
filesForDisplayCb vm = do
	filesList <- koUnwrapObservableList $ files vm
	let filesListDisplay = filter (\fi -> not $ elem (filename fi) [".", ".."]) filesList
	return $ sortBy filesSort filesListDisplay

filesSort :: FileInfo -> FileInfo -> Ordering
filesSort a b
	| filesize a == -1 && filesize b >= 0 = LT
	| filesize a >= 0 && filesize b == -1 = GT
	| otherwise = filenameComp a b
		where filenameComp = textComp `on` filename
