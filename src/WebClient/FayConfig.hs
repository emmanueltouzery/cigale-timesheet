{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RebindableSyntax #-}

import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import JQuery hiding (filter, not, on)
import Prelude hiding ((++), error, putStrLn)
import qualified Prelude as P
import Knockout

import Utils
import FilePicker

(++) = T.append
tshow = T.pack . show
error = P.error . T.unpack
putStrLn = P.putStrLn . T.unpack

jvKeys :: JValue -> [Text]
jvKeys = ffi "getKeys(%1)"

jEmptyValue :: JValue
jEmptyValue = ffi "{}"

jvArrayToObject :: [(Text,Text)] -> JValue
jvArrayToObject = ffi "toJsObject(%1)"

jvValue :: JValue -> Text -> JValue
jvValue = ffi "%1[%2]"

jvValueSet :: JValue -> Text -> Text -> Fay ()
jvValueSet = ffi "%1[%2] = %3"

encodeURIComponent :: Text -> Text
encodeURIComponent = ffi "encodeURIComponent(%1)"

jvArray :: JValue -> [JValue]
jvArray = ffi "%1"

type JvHash = [(Text, JValue)]

jvAsHash :: JValue -> JvHash
jvAsHash value = zip keys values
	where
		keys = jvKeys value
		values = map (jvValue value) keys

jvAsTextHash :: JValue -> [(Text, Text)]
jvAsTextHash = map (\(k,v) -> (k, jvGetString v)) . jvAsHash

-- TODO I would return only Maybe JValue...
-- but somehow I can't make it compile.
-- I think maybe it's because JValue is
-- a phantom type and it has different
-- semantics (notably I shouldn't evaluate it)
jvHashVal :: Text -> JvHash -> Maybe (Text, JValue)
jvHashVal key = find ((==key) . fst) -- >>= (Just . snd)

jvGetString :: JValue -> Text
jvGetString = ffi "%1 + ''"

jClone :: JValue -> JValue
jClone = ffi "JSON.parse(JSON.stringify(%1))"

--- server structs START

data ConfigDataInfo = ConfigDataInfo
	{
		memberName :: Text,
		memberType :: Text
	} deriving (Eq) --, Show)

data PluginConfig = PluginConfig
	{
		cfgPluginName :: Text,
		-- pluginConfig.cfgPluginConfig returns configinfo? stupid naming.
		cfgPluginConfig :: [ConfigDataInfo]
	}
	| InvalidPluginConfig

data ConfigItem = ConfigItem
	{
		configItemName :: T.Text,
		providerName :: T.Text,
		configuration :: JValue
	}
	| InvalidConfigItem

--- server structs END

data ConfigSection = ConfigSection
	{
		pluginInfo :: PluginConfig,
		userSettings :: ObservableList ConfigItem
	}

data ConfigViewModel = ConfigViewModel
	{
		pluginTypes :: ObservableList PluginConfig,
		configSections :: ObservableList ConfigSection,
		pluginContents :: PluginConfig -> ConfigItem -> Fay Text,
		addConfigItem :: ConfigViewModel -> PluginConfig -> Fay (),
		deleteConfigItem :: ConfigSection -> ConfigItem -> Fay (),
		editConfigItem :: ConfigViewModel -> ConfigSection -> ConfigItem -> Fay (),
		modalDialogVM :: Observable ModalDialogVM,
		configAddEditVM :: ConfigAddEditDialogVM
	}

data ModalDialogVM =
	ModalDialogVM
	{
		modalTitle :: Text,
		modalTemplate :: Text,
		actionButtonClass :: Text,
		actionButtonText :: Text,
		errorText :: Observable Text,
		warningText :: Observable Text,
		modalOkClick :: Fay ()
	}
	| InvalidModalDialogVM

data ConfigAddEditDialogVM = ConfigAddEditDialogVM
	{
		pluginBeingEdited :: Observable PluginConfig,
		-- need observable because I'm reusing the
		-- ConfigAddEditDialogVM instead of creating
		-- a new one each time, and if it's not Observable
		-- I can't change it... The view won't change it.
		configurationOriginalValue :: Observable ConfigItem, -- ## i think not needed anymore
		configurationBeingEdited :: Observable ConfigItem,
		passwordType :: Observable Text,
		pluginBeingEditedHasPasswords :: Observable Bool,
		showPasswords :: Observable Bool,
		showFilePickerCb :: PluginConfig -> Observable ConfigItem -> Text -> Fay ()
	}

instance KnockoutModel ConfigViewModel
instance KnockoutModel ModalDialogVM

main :: Fay ()
main = ready $ do
	sectionsObs <- koObservableList []
	pluginTypesObs <- koObservableList []
	let configAddEditVMV = ConfigAddEditDialogVM
		{
			pluginBeingEdited = koObservable InvalidPluginConfig,
			configurationOriginalValue = koObservable InvalidConfigItem,
			configurationBeingEdited = koObservable InvalidConfigItem,
			passwordType = koComputed $ do
				isShowPasswd <- koGet $ showPasswords configAddEditVMV
				return $ if isShowPasswd then "text" else "password",
			pluginBeingEditedHasPasswords = koObservable False,
			showPasswords = koObservable False,
			showFilePickerCb = showFilePickerCallback
		}
	let viewModel = ConfigViewModel
		{
			pluginTypes = pluginTypesObs,
			configSections = sectionsObs,
			pluginContents = pluginContentsCb,
			addConfigItem = \vm cfg -> addEditModuleAction vm cfg Nothing,
			deleteConfigItem = deleteConfigItemCb viewModel,
			editConfigItem = editConfigItemCb,
			modalDialogVM = koObservable InvalidModalDialogVM,
			configAddEditVM = configAddEditVMV
		}
	koApplyBindings viewModel
	myajax2 "/configVal" "/configdesc" $ \val desc ->
		handleValDesc viewModel (head val) (head desc)

showFilePickerCallback :: PluginConfig -> Observable ConfigItem -> Text -> Fay ()
showFilePickerCallback pluginCfg configurationBeingEdited memberNameV = do
	let memberTypeV = memberType $ fromJust 
		$ find ((==memberNameV) . memberName) (cfgPluginConfig pluginCfg)
	configurationBeingEditedV <- configuration <$> koGet configurationBeingEdited
	let maybeCurPath = lookup memberNameV (jvAsHash configurationBeingEditedV)
	let curPath = case maybeCurPath of
		Just v -> jvGetString v
		Nothing -> ""
	let opMode = case memberTypeV of
		"FilePath" -> PickFile
		"FolderPath" -> PickFolder
	let pickerOptions = pickerDefaultOptions
		{
			pickerMode = opMode
		}
	showFilePicker curPath pickerOptions (pickerFileChanged memberNameV configurationBeingEdited)

handleValDesc :: ConfigViewModel -> [ConfigItem] -> [PluginConfig] -> Fay ()
handleValDesc vm configItems pluginConfigs = do
	let cfgByProvider = bucketsT providerName configItems
	foldM_ (addConfigSection pluginConfigs) (configSections vm) cfgByProvider
	koPushAllObservableList (pluginTypes vm) pluginConfigs
	return ()

-- monomorphic, no typeclasses in fay...
bucketsT :: (a -> T.Text) -> [a] -> [(T.Text, [a])]
bucketsT f = map (\g -> (fst $ head g, map snd g))
          . groupBy ((==) `on` fst)
          . sortBy (strComp `on` (T.unpack . fst))
          . map (\x -> (f x, x))

addConfigSection :: [PluginConfig] -> ObservableList ConfigSection -> (T.Text, [ConfigItem]) -> Fay (ObservableList ConfigSection)
addConfigSection pluginConfigs soFar configValue = getConfigSection configValue pluginConfigs
	>>= koPushObservableList soFar >> return soFar

getConfigSection :: (Text, [ConfigItem]) -> [PluginConfig] -> Fay ConfigSection
getConfigSection configValue pluginConfigs = do
		userSettingsL <- koObservableList $ sortBy 
			(strComp `on` (T.unpack . configItemName)) $ snd configValue
		return ConfigSection
			{
				pluginInfo = pluginConfig,
				userSettings = userSettingsL
			}
	where
		mPluginConfig = find ((== fst configValue) . cfgPluginName) pluginConfigs
		pluginConfig = fromMaybe
			(error $ "The app doesn't know about plugin type " ++ fst configValue)
			mPluginConfig

getNewSourceName :: ConfigViewModel -> PluginConfig -> Fay T.Text
getNewSourceName vm pluginConfig = do
	allSections <- koUnwrapObservableList (configSections vm) 
	allUserSettings <- concatMapM koUnwrapObservableList $ map userSettings allSections
	let existingNames = map configItemName allUserSettings
	return $ findNewName (cfgPluginName pluginConfig) existingNames

findNewName :: Text -> [Text] -> Text
findNewName base existing = tryNextName base existing (1::Int)
	where tryNextName b e i = if isNothing (find (==attempt) existing)
		then attempt
		else tryNextName b e (i+1)
		where attempt = base ++ " #" ++ T.pack (show i)

-- hmm... doesn't the configsection include the jvalue?
addEditModuleAction :: ConfigViewModel -> PluginConfig -> Maybe (ConfigSection, ConfigItem) -> Fay ()
addEditModuleAction vm pluginConfig maybeConfigValue = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	let cfgAddEditVm = configAddEditVM vm
	pluginConfig ~> pluginBeingEdited cfgAddEditVm
	hasPasswords pluginConfig ~> pluginBeingEditedHasPasswords cfgAddEditVm
	case maybeConfigValue of
		Just configValue -> do
			snd configValue ~~> configurationBeingEdited cfgAddEditVm
			snd configValue ~~> configurationOriginalValue cfgAddEditVm
		Nothing -> do
			let blankValue = map ((\x -> (x, "")) . memberName) $ cfgPluginConfig pluginConfig
			newSrcName <- getNewSourceName vm pluginConfig
			let blankCfg = ConfigItem newSrcName pluginName $ jvArrayToObject blankValue
			blankCfg ~> configurationBeingEdited cfgAddEditVm
			blankCfg ~> configurationOriginalValue cfgAddEditVm
	let warningTextV = if hasPasswords pluginConfig
		then "Warning: passwords are stored in plain text in the configuration file!"
		else ""
	let clickAction = addEditModuleClick (cfgPluginConfig pluginConfig) vm maybeConfigValue
	modalVM <- prepareModal (Primary "Save changes") pluginName
		"configEditTemplate" clickAction modal vm
 	warningTextV ~> warningText modalVM
	bootstrapModal modal

addEditModuleClick :: [ConfigDataInfo] -> ConfigViewModel -> Maybe (ConfigSection, ConfigItem) -> Fay ()
addEditModuleClick cfgPlConfig vm maybeConfigValue = do
	let cfgAddEditVm = configAddEditVM vm
	pluginConfig <- koGet $ pluginBeingEdited cfgAddEditVm
	let memberNames = map memberName $ cfgPlConfig
	let pluginName = cfgPluginName pluginConfig
	let cfgAddEditVm = configAddEditVM vm
	newConfigItem <- koGet (configurationBeingEdited $ configAddEditVM vm)
	if not $ validateEntry memberNames (jvAsTextHash $ configuration newConfigItem)
		then do
			mVm <- koGet $ modalDialogVM vm
			"Please fill in all the fields" ~> warningText mVm
		else case maybeConfigValue of
			Nothing -> addPluginConfig vm pluginConfig
			Just (configSection, existingConfig) ->
				koGet (configurationOriginalValue cfgAddEditVm) >>=
					updatePluginConfig vm configSection pluginName

validateEntry :: [Text] -> [(Text,Text)] -> Bool
validateEntry [] _ = True
validateEntry (x:xs) newConfig = (isJust $ find (\(k,v) -> k == x && not (T.null v)) newConfig)
	&& validateEntry xs newConfig

hasPasswords :: PluginConfig -> Bool
hasPasswords = hasMemberType (== "Password")

hasMemberType :: (Text -> Bool) -> PluginConfig -> Bool
hasMemberType predicate pluginCfg = isJust $ find (predicate . memberType) (cfgPluginConfig pluginCfg)

data MainAction = Primary Text
		  | Danger Text

-- TODO remove the modal parameter,
-- in the end all must be through knockout.
prepareModal :: MainAction -> Text -> Text -> Fay () -> JQuery -> ConfigViewModel -> Fay ModalDialogVM
prepareModal action title template clickCallback modal vm = do
	let modalV = ModalDialogVM
		{
			modalTitle = title,
			modalTemplate = template,
			modalOkClick = clickCallback,
			actionButtonClass = "btn btn-" ++ btnType,
			actionButtonText = actionText,
			errorText = koObservable "",
			warningText = koObservable ""
		}
	modalV ~> modalDialogVM vm
	return modalV
	where
		(btnType, actionText) = case action of
			Primary x -> ("primary", x)
			Danger x -> ("danger", x)
			_ -> error $ "Unknown action: " ++ tshow action

deleteConfigItemCb :: ConfigViewModel -> ConfigSection -> ConfigItem -> Fay ()
deleteConfigItemCb vm section userSetting = do
	let pluginName = cfgPluginName $ pluginInfo section
	modal <- select "#myModal"
	prepareModal (Danger "Delete") pluginName "confirmDeleteTemplate"
		(deleteConfigItemAction vm section userSetting) modal vm
	bootstrapModal modal
	return ()

deleteConfigItemAction :: ConfigViewModel -> ConfigSection -> ConfigItem -> Fay ()
deleteConfigItemAction vm section userSetting = do
	let pluginName = cfgPluginName $ pluginInfo section
	let parm = jvGetString $ jqParam (tshow userSetting)
	let url = "/config?configItemName=" ++ (encodeURIComponent $ configItemName userSetting)
	ajxDelete url (showError vm) $ do
		koFilter ((/=configItemName userSetting) . configItemName) (userSettings section)
		closePopup

editConfigItemCb :: ConfigViewModel -> ConfigSection -> ConfigItem -> Fay ()
editConfigItemCb vm section userSetting =
	addEditModuleAction vm (pluginInfo section) (Just (section, userSetting))

updatePluginConfig :: ConfigViewModel -> ConfigSection -> Text -> ConfigItem -> Fay ()
updatePluginConfig vm configSection pluginName oldConfig = do
	newConfigObj <- koGet (configurationBeingEdited $ configAddEditVM vm)
	let url = "/config?pluginName=" ++ pluginName ++ "&oldConfigItemName=" ++ (encodeURIComponent $ configItemName oldConfig)
	ajxPut url newConfigObj (showError vm) $ do
		koFilter ((/=configItemName oldConfig) . configItemName) (userSettings configSection)
		koPushObservableList (userSettings configSection) newConfigObj
		koSort configItemName (userSettings configSection)
		closePopup

addPluginConfig :: ConfigViewModel -> PluginConfig -> Fay ()
addPluginConfig vm pluginConfig = do
	newConfigInfo <- koGet (configurationBeingEdited $ configAddEditVM vm)
	let pluginName = providerName newConfigInfo
	let newConfigObj = newConfigInfo --ConfigItem (getNewSourceName pluginConfig) pluginName newConfigJValue
	ajxPost ("/config?pluginName=" ++ pluginName) newConfigObj (showError vm)
		(addPluginInVm vm pluginConfig newConfigInfo >> closePopup)

addPluginInVm :: ConfigViewModel -> PluginConfig -> ConfigItem -> Fay ()
addPluginInVm vm pluginConfig newConfigItem = do
	section <- findOrCreateSection vm pluginConfig
	koPushObservableList (userSettings section) newConfigItem

findOrCreateSection :: ConfigViewModel -> PluginConfig -> Fay ConfigSection
findOrCreateSection vm pluginConfig = do
	sections <- koUnwrapObservableList (configSections vm)
	let mSection = find ((==cfgPluginName pluginConfig) . cfgPluginName . pluginInfo) sections
	case mSection of
		Just section -> return section
		Nothing -> do
			-- no such section, create it.
			userSettingsL <- koObservableList []
			let newSection = ConfigSection {pluginInfo = pluginConfig, userSettings=userSettingsL}
			koPushObservableList (configSections vm) newSection
			return newSection

closePopup :: Fay ()
closePopup = select "#myModal" >>= bootstrapModalHide

showError :: ConfigViewModel -> Fay ()
showError vm = do
	modalVM <- koGet $ modalDialogVM vm
	"Error applying the change!" ~> errorText modalVM

getPluginElementHtml :: JValue -> ConfigDataInfo -> Fay Text
getPluginElementHtml config dataInfo = do
	let memberNameV = memberName dataInfo
	let memberValue = jvGetString (jvValue config memberNameV)
	let memberValueDisplay = case memberType dataInfo of
		"Password" -> T.pack $ replicate (T.length memberValue) '*'
		_ -> memberValue
	return $ memberNameV ++ " " ++ memberValueDisplay

pluginContentsCb :: PluginConfig -> ConfigItem -> Fay Text
pluginContentsCb pluginConfig configContents = do
	htmlList <- mapM (getPluginElementHtml $ configuration configContents) (cfgPluginConfig pluginConfig)
	return $ "<div>" ++ T.intercalate "</div><div>" htmlList ++ "</div>"

pickerFileChanged :: Text -> Observable ConfigItem -> Text -> Fay ()
pickerFileChanged memberName configurationBeingEdited path = do
	configurationBeingEditedV <- koGet configurationBeingEdited
	jvValueSet (configuration configurationBeingEditedV) memberName path
	configurationBeingEditedV ~> configurationBeingEdited
	

-- http://stackoverflow.com/questions/18025474/multiple-ajax-queries-in-parrallel-with-fay
myajax2 :: Text -> Text -> (Automatic b -> Automatic c -> Fay ()) -> Fay ()
myajax2 = ffi "$.when($.getJSON(%1), $.getJSON(%2)).then(%3)"
