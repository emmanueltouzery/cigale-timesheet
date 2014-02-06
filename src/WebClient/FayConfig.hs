{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RebindableSyntax #-}

import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import JQuery hiding (filter)
import Prelude hiding ((++), error, putStrLn)
import qualified Prelude as P
import Knockout

import Utils
import FilePicker (getFolderContents, showFilePicker, FileInfo(..), ClientFileInfo(..))

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

data PluginConfig =
	PluginConfig
	{
		cfgPluginName :: Text,
		-- pluginConfig.cfgPluginConfig returns configinfo? stupid naming.
		cfgPluginConfig :: [ConfigDataInfo]
	}
	| InvalidPluginConfig

--- server structs END

data ConfigSection = ConfigSection
	{
		pluginInfo :: PluginConfig,
		userSettings :: ObservableList JValue
	}

-- TODO i don't like JValue in the view model
data ConfigViewModel = ConfigViewModel
	{
		pluginTypes :: ObservableList PluginConfig,
		configSections :: ObservableList ConfigSection,
		pluginContents :: PluginConfig -> JValue -> Fay Text,
		addConfigItem :: ConfigViewModel -> PluginConfig -> Fay (),
		deleteConfigItem :: ConfigSection -> JValue -> Fay (),
		editConfigItem :: ConfigViewModel -> ConfigSection -> JValue -> Fay (),
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
		configurationOriginalValue :: Observable JValue,
		configurationBeingEdited :: Observable JValue,
		passwordType :: Observable Text,
		pluginBeingEditedHasPasswords :: Observable Bool,
		showPasswords :: Observable Bool,
		showFilePickerCb :: Text -> Observable JValue -> Fay ()
	}

instance KnockoutModel ConfigViewModel

main :: Fay ()
main = ready $ do
	sectionsObs <- koObservableList []
	pluginTypesObs <- koObservableList []
	let configAddEditVMV = ConfigAddEditDialogVM
		{
			pluginBeingEdited = koObservable InvalidPluginConfig,
			configurationOriginalValue = koObservable jEmptyValue,
			configurationBeingEdited = koObservable jEmptyValue,
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

showFilePickerCallback :: Text -> Observable JValue -> Fay ()
showFilePickerCallback memberName configurationBeingEdited = do
	configurationBeingEditedV <- koGet configurationBeingEdited
	let curPath = jvGetString $ jvValue configurationBeingEditedV memberName
	print curPath
	showFilePicker curPath (pickerFileChanged memberName configurationBeingEdited)

handleValDesc :: ConfigViewModel -> JValue -> [PluginConfig] -> Fay ()
handleValDesc vm configVal pluginConfigs = do
	let hash = jvAsHash configVal
	-- TODO probably need a proper Text comparison...
	let sortedHash = sortBy (\(a, _) (b, _) -> strComp (T.unpack a) (T.unpack b)) hash
	-- TODO too long line
	foldM_ (\soFar configValue -> getConfigSection configValue pluginConfigs >>= koPushObservableList soFar >> return soFar) (configSections vm) sortedHash
	koPushAllObservableList (pluginTypes vm) pluginConfigs
	configSections <- koUnwrapObservableList (configSections vm)
	return ()

getConfigSection :: (Text, JValue) -> [PluginConfig] -> Fay ConfigSection
getConfigSection configValue pluginConfigs = do
		userSettingsL <- koObservableList userSettingsList
		return ConfigSection
			{
				pluginInfo = pluginConfig,
				userSettings = userSettingsL
			}
	where
		userSettingsList = jvArray $ snd configValue
		mPluginConfig = find ((== fst configValue) . cfgPluginName) pluginConfigs
		pluginConfig = fromMaybe
			(error $ "The app doesn't know about plugin type " ++ fst configValue)
			mPluginConfig

-- hmm... doesn't the configsection include the jvalue?
addEditModuleAction :: ConfigViewModel -> PluginConfig -> Maybe (ConfigSection, JValue) -> Fay ()
addEditModuleAction vm pluginConfig maybeConfigValue = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	let configMembers = cfgPluginConfig pluginConfig
	let cfgAddEditVm = configAddEditVM vm
	pluginConfig ~> pluginBeingEdited cfgAddEditVm
	hasPasswords pluginConfig ~> pluginBeingEditedHasPasswords cfgAddEditVm
	case maybeConfigValue of
		Just configValue -> do
			snd configValue ~> configurationBeingEdited cfgAddEditVm
			jClone (snd configValue) ~> configurationOriginalValue cfgAddEditVm
		Nothing -> do
			jEmptyValue ~> configurationBeingEdited cfgAddEditVm
			jEmptyValue ~> configurationOriginalValue cfgAddEditVm
	let clickCallback = case maybeConfigValue of
		Nothing -> addPluginConfig vm pluginConfig
		Just (configSection, existingConfig) ->
			koGet (configurationOriginalValue cfgAddEditVm) >>=
				updatePluginConfig vm configSection pluginName
	let warningTextV = if hasPasswords pluginConfig
		then "Warning: passwords are stored in plain text in the configuration file!"
		else ""
	modalVM <- prepareModal (Primary "Save changes") pluginName "configEditTemplate" clickCallback modal vm
 	warningTextV ~> warningText modalVM
	bootstrapModal modal

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

deleteConfigItemCb :: ConfigViewModel -> ConfigSection -> JValue -> Fay ()
deleteConfigItemCb vm section userSetting = do
	let pluginName = cfgPluginName $ pluginInfo section
	modal <- select "#myModal"
	prepareModal (Danger "Delete") pluginName "confirmDeleteTemplate"
		(deleteConfigItemAction vm section userSetting) modal vm
	bootstrapModal modal
	return ()

deleteConfigItemAction :: ConfigViewModel -> ConfigSection -> JValue -> Fay ()
deleteConfigItemAction vm section userSetting = do
	let pluginName = cfgPluginName $ pluginInfo section
	let parm = jvGetString $ jqParam (tshow userSetting)
	let url = "/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm
	ajxDelete url (showError vm) $ do
		koRemoveObservableList (userSettings section) userSetting
		closePopup

editConfigItemCb :: ConfigViewModel -> ConfigSection -> JValue -> Fay ()
editConfigItemCb vm section userSetting =
	addEditModuleAction vm (pluginInfo section) (Just (section, userSetting))

updatePluginConfig :: ConfigViewModel -> ConfigSection -> Text -> JValue -> Fay ()
updatePluginConfig vm configSection pluginName oldConfig = do
	newConfigJValue <- koGet (configurationBeingEdited $ configAddEditVM vm)
	let newConfig = jvAsTextHash newConfigJValue
	let newConfigObj = jvArrayToObject newConfig
	let parm = jvGetString $ jqParam (tshow oldConfig)
	let url = "/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm
	ajxPut url newConfigObj (showError vm) $ do
		koReplaceElementObservableList (userSettings configSection) oldConfig newConfigObj
		closePopup

addPluginConfig :: ConfigViewModel -> PluginConfig -> Fay ()
addPluginConfig vm pluginConfig = do
	newConfigJValue <- koGet (configurationBeingEdited $ configAddEditVM vm)
	let newConfig = jvAsTextHash newConfigJValue
	let pluginName = cfgPluginName pluginConfig
	let newConfigObj = jvArrayToObject newConfig
	ajxPost ("/config?pluginName=" ++ pluginName) newConfigObj (showError vm)
		(addPluginInVm vm pluginConfig newConfig >> closePopup)

addPluginInVm :: ConfigViewModel -> PluginConfig -> [(Text,Text)] -> Fay ()
addPluginInVm vm pluginConfig newConfig = do
	section <- findOrCreateSection vm pluginConfig
	koPushObservableList (userSettings section) (jvArrayToObject newConfig)

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

pluginContentsCb :: PluginConfig -> JValue -> Fay Text
pluginContentsCb pluginConfig configContents = do
	htmlList <- mapM (getPluginElementHtml configContents) (cfgPluginConfig pluginConfig)
	return $ "<div>" ++ T.intercalate "</div><div>" htmlList ++ "</div>"

pickerFileChanged :: Text -> Observable JValue -> ClientFileInfo -> Fay ()
pickerFileChanged memberName configurationBeingEdited fileInfo = do
	let path = filename $ serverInfo fileInfo
	putStrLn path
	configurationBeingEditedV <- koGet configurationBeingEdited
	jvValueSet configurationBeingEditedV memberName path
	configurationBeingEditedV ~> configurationBeingEdited
	

-- http://stackoverflow.com/questions/18025474/multiple-ajax-queries-in-parrallel-with-fay
myajax2 :: Text -> Text -> (Automatic b -> Automatic c -> Fay ()) -> Fay ()
myajax2 = ffi "$.when($.getJSON(%1), $.getJSON(%2)).then(%3)"
