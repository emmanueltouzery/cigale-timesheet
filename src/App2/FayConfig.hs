{-# LANGUAGE NoImplicitPrelude, EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}

import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import JQuery hiding (filter)
import Prelude hiding ((++), error, putStrLn)
import qualified Prelude as P
import Knockout

import Utils

data JValue

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
jvHashVal key hash = find ((==key) . fst) hash -- >>= (Just . snd)

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
		userSettings :: ObservableArray JValue
	}

-- TODO modal click callback also through VM ###

-- TODO i don't like JValue in the view model
data ConfigViewModel = ConfigViewModel
	{
		pluginTypes :: ObservableArray PluginConfig,
		configSections :: ObservableArray ConfigSection,
		pluginContents :: PluginConfig -> JValue -> Fay Text,
		addConfigItem :: ConfigViewModel -> PluginConfig -> Fay (),
		deleteConfigItem :: ConfigSection -> JValue -> Fay (),
		editConfigItem :: ConfigViewModel -> ConfigSection -> JValue -> Fay (),
		modalDialogVM :: ModalDialogVM,
		configAddEditVM :: ConfigAddEditDialogVM
	}

data ModalDialogVM = ModalDialogVM
	{
		modalTitle :: Observable Text,
		modalTemplate :: Observable Text,
		actionButtonClass :: Observable Text,
		actionButtonText :: Observable Text
	}

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
		showPasswords :: Observable Bool
	}

instance KnockoutModel ConfigViewModel

main :: Fay ()
main = ready $ do
	sectionsObs <- ko_observableList []
	pluginTypesObs <- ko_observableList []
	let configAddEditVMV = ConfigAddEditDialogVM
		{
			pluginBeingEdited = ko_observable InvalidPluginConfig,
			configurationOriginalValue = ko_observable jEmptyValue,
			configurationBeingEdited = ko_observable jEmptyValue,
			passwordType = ko_computed $ do
				isShowPasswd <- ko_get $ showPasswords configAddEditVMV
				return $ if isShowPasswd then "text" else "password",
			pluginBeingEditedHasPasswords = ko_observable False,
			showPasswords = ko_observable False
		}
	let modalDialogVMV = ModalDialogVM
		{
			modalTitle = ko_observable "",
			modalTemplate = ko_observable "",
			actionButtonClass = ko_observable "",
			actionButtonText = ko_observable ""
		}
	let viewModel = ConfigViewModel
		{
			pluginTypes = pluginTypesObs,
			configSections = sectionsObs,
			pluginContents = pluginContentsCb,
			addConfigItem = \vm cfg -> addEditModuleAction vm cfg Nothing,
			deleteConfigItem = deleteConfigItemCb viewModel,
			editConfigItem = editConfigItemCb,
			modalDialogVM = modalDialogVMV,
			configAddEditVM = configAddEditVMV
		}
	ko_applyBindings viewModel
	myajax2 "/configVal" "/configdesc" $ \val desc ->
		handleValDesc viewModel (head val) (head desc)

handleValDesc :: ConfigViewModel -> JValue -> [PluginConfig] -> Fay ()
handleValDesc vm configVal pluginConfigs = do
	let hash = jvAsHash configVal
	-- TODO probably need a proper Text comparison...
	let sortedHash = sortBy (\(a, _) (b, _) -> strComp (T.unpack a) (T.unpack b)) hash
	-- TODO too long line
	foldM_ (\soFar configValue -> getConfigSection configValue pluginConfigs >>= ko_pushObservableArray soFar >> return soFar) (configSections vm) sortedHash
	ko_pushAllObservableArray (pluginTypes vm) pluginConfigs
	configSections <- ko_unwrapObservableArray (configSections vm)
	return ()

getConfigSection :: (Text, JValue) -> [PluginConfig] -> Fay ConfigSection
getConfigSection configValue pluginConfigs = do
		userSettingsL <- ko_observableList userSettingsList
		return ConfigSection
			{
				pluginInfo = pluginConfig,
				userSettings = userSettingsL
			}
	where
		userSettingsList = jvArray $ snd configValue
		mPluginConfig = find ((== fst configValue) . cfgPluginName) pluginConfigs
		pluginConfig = case mPluginConfig of
			Just x -> x
			Nothing -> error $ "The app doesn't know about plugin type " ++ (fst configValue)

-- hmm... doesn't the configsection include the jvalue?
addEditModuleAction :: ConfigViewModel -> PluginConfig -> Maybe (ConfigSection, JValue) -> Fay ()
addEditModuleAction vm pluginConfig maybeConfigValue = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	ko_set (modalTitle $ modalDialogVM vm) pluginName
	prepareModal (Primary "Save changes") modal vm
	let configMembers = cfgPluginConfig pluginConfig
	let cfgAddEditVm = configAddEditVM vm
	ko_set (pluginBeingEdited cfgAddEditVm) pluginConfig
	ko_set (pluginBeingEditedHasPasswords cfgAddEditVm) (hasPasswords pluginConfig)
	case maybeConfigValue of
		Just configValue -> do
			ko_set (configurationBeingEdited cfgAddEditVm) (snd configValue)
			ko_set (configurationOriginalValue cfgAddEditVm) (jClone $ snd configValue)
		Nothing -> do
			ko_set (configurationBeingEdited cfgAddEditVm) jEmptyValue
			ko_set (configurationOriginalValue cfgAddEditVm) jEmptyValue
	ko_set (modalTemplate $ modalDialogVM vm) "configEditTemplate"
	let clickCallback = case maybeConfigValue of
		Nothing -> addPluginConfig vm pluginConfig
		Just (configSection, existingConfig) ->
			(ko_get $ configurationOriginalValue cfgAddEditVm) >>=
				updatePluginConfig vm configSection pluginName
	findSelector "button#main-action" modal >>= click (\_ -> clickCallback) -- pluginConfig config)
	bootstrapModal modal

hasPasswords :: PluginConfig -> Bool
hasPasswords pluginCfg = isJust $ find ((== "Password") . memberType) (cfgPluginConfig pluginCfg)

data MainAction = Primary Text
		  | Danger Text

-- TODO remove the modal parameter,
-- in the end all must be through knockout.
prepareModal :: MainAction -> JQuery -> ConfigViewModel -> Fay ()
prepareModal action modal vm = do
	select "div#error" >>= hide Instantly
	ko_set (actionButtonClass $ modalDialogVM vm) $ "btn btn-" ++ btnType
	ko_set (actionButtonText $ modalDialogVM vm) actionText
	where
		(btnType, actionText) = case action of
			Primary x -> ("primary", x)
			Danger x -> ("danger", x)
			_ -> error $ "Unknown action: " ++ (tshow action)

bootstrapModal :: JQuery -> Fay ()
bootstrapModal = ffi "%1.modal('show')"

bootstrapModalHide :: JQuery -> Fay ()
bootstrapModalHide = ffi "%1.modal('hide')"

deleteConfigItemCb :: ConfigViewModel -> ConfigSection -> JValue -> Fay ()
deleteConfigItemCb vm section userSetting = do
	let pluginName = cfgPluginName $ pluginInfo section
	modal <- select "#myModal"
	ko_set (modalTitle $ modalDialogVM vm) pluginName
	prepareModal (Danger "Delete") modal vm
	ko_set (modalTemplate $ modalDialogVM vm) "confirmDeleteTemplate"
	bootstrapModal modal
	findSelector "button#main-action" modal >>= click (\_ -> deleteConfigItemAction section userSetting)
	return ()

deleteConfigItemAction :: ConfigSection -> JValue -> Fay ()
deleteConfigItemAction section userSetting = do
	let pluginName = cfgPluginName $ pluginInfo section
	let parm = jvGetString $ jqParam (tshow userSetting)
	let url = "/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm
	ajxDelete url $ do
		ko_removeObservableArray (userSettings section) userSetting
		closePopup

editConfigItemCb :: ConfigViewModel -> ConfigSection -> JValue -> Fay ()
editConfigItemCb vm section userSetting = do
	addEditModuleAction vm (pluginInfo section) (Just (section, userSetting))

updatePluginConfig :: ConfigViewModel -> ConfigSection -> Text -> JValue -> Fay ()
updatePluginConfig vm configSection pluginName oldConfig = do
	newConfigJValue <- ko_get (configurationBeingEdited $ configAddEditVM vm)
	let newConfig = jvAsTextHash newConfigJValue
	let newConfigObj = jvArrayToObject newConfig
	let parm = jvGetString $ jqParam (tshow $ oldConfig)
	ajxPut ("/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm) newConfigObj $ do
		ko_replaceElementObservableArray (userSettings configSection) oldConfig newConfigObj
		closePopup

addPluginConfig :: ConfigViewModel -> PluginConfig -> Fay ()
addPluginConfig vm pluginConfig = do
	newConfigJValue <- ko_get (configurationBeingEdited $ configAddEditVM vm)
	let newConfig = jvAsTextHash newConfigJValue
	let pluginName = cfgPluginName pluginConfig
	let newConfigObj = jvArrayToObject newConfig
	ajxPost ("/config?pluginName=" ++ pluginName) newConfigObj (addPluginInVm vm pluginConfig newConfig >> closePopup)

addPluginInVm :: ConfigViewModel -> PluginConfig -> [(Text,Text)] -> Fay ()
addPluginInVm vm pluginConfig newConfig = do
	section <- findOrCreateSection vm pluginConfig
	ko_pushObservableArray (userSettings section) (jvArrayToObject newConfig)

findOrCreateSection :: ConfigViewModel -> PluginConfig -> Fay ConfigSection
findOrCreateSection vm pluginConfig = do
	sections <- ko_unwrapObservableArray (configSections vm)
	let mSection = find ((==cfgPluginName pluginConfig) . cfgPluginName . pluginInfo) sections
	case mSection of
		Just section -> return section
		Nothing -> do
			-- no such section, create it.
			userSettingsL <- ko_observableList []
			let newSection = ConfigSection {pluginInfo = pluginConfig, userSettings=userSettingsL}
			ko_pushObservableArray (configSections vm) newSection
			return newSection

closePopup :: Fay ()
closePopup = do
	select "#myModal" >>= bootstrapModalHide

ajxPut :: Text -> JValue -> Fay () -> Fay ()
ajxPut = ffi "jQuery.ajax({type:'PUT', url: %1, data: JSON.stringify(%2)}).success(%3).fail($('div#error').show())"

ajxPost :: Text -> JValue -> Fay () -> Fay ()
ajxPost = ffi "jQuery.ajax({type:'POST', url: %1, data: JSON.stringify(%2)}).success(%3).fail($('div#error').show())"

ajxDelete :: Text -> Fay () -> Fay ()
ajxDelete = ffi "jQuery.ajax({type:'DELETE', url: %1}).success(%2).fail($('div#error').show())"

jqParam :: Text -> JValue
--jqParam = ffi "jQuery.param(%1)"
jqParam = ffi "encodeURIComponent(%1)"

getPluginElementHtml :: JValue -> ConfigDataInfo -> Fay Text
getPluginElementHtml config dataInfo = do
	let memberNameV = memberName dataInfo
	let memberValue = jvGetString (jvValue config memberNameV)
	let memberValueDisplay = case (memberType dataInfo) of
		"Password" -> T.pack $ replicate (T.length memberValue) '*'
		_ -> memberValue
	return $ memberNameV ++ " " ++ memberValueDisplay

pluginContentsCb :: PluginConfig -> JValue -> Fay Text
pluginContentsCb pluginConfig configContents = do
	htmlList <- mapM (getPluginElementHtml configContents) (cfgPluginConfig pluginConfig)
	return $ "<div>" ++ T.intercalate "</div><div>" htmlList ++ "</div>"

-- http://stackoverflow.com/questions/18025474/multiple-ajax-queries-in-parrallel-with-fay
myajax2 :: Text -> Text -> (Automatic b -> Automatic c -> Fay ()) -> Fay ()
myajax2 = ffi "$.when($.getJSON(%1), $.getJSON(%2)).then(%3)"
