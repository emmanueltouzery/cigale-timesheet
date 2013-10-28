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

jvArrayToObject :: [(Text,Text)] -> JValue
jvArrayToObject = ffi "toJsObject(%1)"

--jvValue :: JValue -> T.Text -> JValue
--jvValue v k = jvValue' v (T.unpack k)
--
--jvValue' :: JValue -> String -> JValue
--jvValue' = ffi "%1[%2]"

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

-- TODO I would return only Maybe JValue...
-- but somehow I can't make it compile.
-- I think maybe it's because JValue is
-- a phantom type and it has different
-- semantics (notably I shouldn't evaluate it)
jvHashVal :: Text -> JvHash -> Maybe (Text, JValue)
jvHashVal key hash = find ((==key) . fst) hash -- >>= (Just . snd)

jvGetString :: JValue -> Text
jvGetString = ffi "%1 + ''"

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

--- server structs END

data ConfigSection = ConfigSection
	{
		pluginInfo :: PluginConfig,
		userSettings :: ObservableArray JValue
	}

data ConfigViewModel = ConfigViewModel
	{
		pluginTypes :: ObservableArray PluginConfig,
		configSections :: ObservableArray ConfigSection,
		pluginContents :: PluginConfig -> JValue -> Fay Text,
		addConfigItem :: ConfigViewModel -> PluginConfig -> Fay (),
		deleteConfigItem :: ConfigSection -> JValue -> Fay (),
		editConfigItem :: ConfigViewModel -> ConfigSection -> JValue -> Fay ()
	}
instance KnockoutModel ConfigViewModel

data FormEntryProvider = FormEntryProvider
	{
		prvServerTypes :: [Text],
		prvGetHtml :: Text -> Text -> Text,
		prvGetValue :: JQuery -> Text -> Fay Text,
		prvCreateCallback :: Text -> Fay JQuery -> Fay ()
	}
basicEntryProvider = FormEntryProvider
	{
		prvServerTypes = undefined,
		prvGetHtml = undefined,
		prvGetValue = \sel mName -> findSelector ("input#" ++ mName) sel >>= getVal,
		prvCreateCallback = \_ _ -> return ()
	}

stringEntryProvider = basicEntryProvider
	{
		prvServerTypes = ["String", "Text", "ByteString", "FilePath"],
		prvGetHtml = getHtml
	}
	where
		getHtml memberName curValue = textReplaceAll "{}" memberName (
				"<label for='{}'>{}</label><input type='text'"
				++ " class='form-control' id='{}' placeholder='Enter {}' value='"
				++ curValue ++ "'></div>")

passwordEntryProvider = basicEntryProvider
	{
		prvServerTypes = ["Password"],
		prvGetHtml = getHtml,
		prvCreateCallback = handleCb
	}
	where
		getHtml memberName curValue = textReplaceAll "{}" memberName (
				"<label for='{}'>{}</label><input type='password'"
				++ " class='form-control' id='{}' placeholder='Enter {}' value='"
				++ curValue ++ "'><label><input type='checkbox' id='cb-{}'/>"
				++ "Show password</label></div>")
		handleCb memberName jqSel = jqSel >>= findSelector ("input#cb-" ++ memberName)
					>>= click (\_ -> toggleShowPass jqSel memberName)
					>> return ()
		toggleShowPass jqSel memberName = do
				input <- jqSel >>= findSelector ("input#" ++ memberName)
				otherType <- getOtherType input
				setAttr "type" otherType input
				return ()
		getOtherType input = do
				typeStr <- getAttr "type" input
				return $ if typeStr == "text" then "password" else "text"

formEntryProviders :: [FormEntryProvider]
formEntryProviders = [stringEntryProvider, passwordEntryProvider]

formEntryForType :: Text -> Maybe FormEntryProvider
formEntryForType serverType = find (\p -> serverType `elem` prvServerTypes p) formEntryProviders

main :: Fay ()
main = ready $ do
	sectionsObs <- ko_observableList []
	pluginTypesObs <- ko_observableList []
	let viewModel = ConfigViewModel
		{
			pluginTypes = pluginTypesObs,
			configSections = sectionsObs,
			pluginContents = pluginContentsCb,
			addConfigItem = \vm cfg -> addEditModuleAction vm cfg Nothing,
			deleteConfigItem = deleteConfigItemCb,
			editConfigItem = editConfigItemCb
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

-- TODO get rid of the double maybe
addEditModuleAction :: ConfigViewModel -> PluginConfig -> Maybe (ConfigSection, JValue) -> Fay ()
addEditModuleAction vm pluginConfig maybeConfigValue = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	findSelector "div.modal-header h4" modal >>= setText pluginName
	prepareModal (Primary "Save changes") modal
	let configMembers = cfgPluginConfig pluginConfig
	(modalContents, callbacks) <- getModalContents configMembers (liftMaybe snd maybeConfigValue)
	let contentsSelector = findSelector "div.modal-body" modal
	contentsSelector >>= setHtml modalContents
	putStrLn "running callbacks"
	mapM_ ($ contentsSelector) callbacks
	putStrLn "ran callbacks"
	let clickCallback enteredData = case maybeConfigValue of
		Nothing -> addPluginConfig vm pluginConfig enteredData
		Just (configSection, existingConfig) -> updatePluginConfig vm configSection pluginName enteredData existingConfig
	findSelector "button#main-action" modal >>= click (\_ -> getModalEnteredData configMembers modal >>= clickCallback) -- pluginConfig config)
	bootstrapModal modal

data MainAction = Primary Text
		  | Danger Text

prepareModal :: MainAction -> JQuery -> Fay JQuery
prepareModal action modal = do
	select "div#error" >>= hide Instantly
	footer <- findSelector "div.modal-footer" modal
	findSelector "#main-action" footer >>= remove
	let btnHtml = "<button type='button' id='main-action' class='btn btn-" ++ btnType ++ "'>" ++ actionText ++ "</button>"
	(select btnHtml) >>= appendTo footer
	where
		(btnType, actionText) = case action of
			Primary x -> ("primary", x)
			Danger x -> ("danger", x)
			_ -> error $ "Unknown action: " ++ (tshow action)

getModalContents :: [ConfigDataInfo] -> Maybe JValue -> Fay (Text, [Fay JQuery -> Fay()])
getModalContents types config = do
		let configHash = maybe [] jvAsHash config
		formContents <- sequence $ map (getConfigDataInfoForm configHash) types 
		let str = "<form role='form'><div class='form-group'>"
			++ (foldr (++) "" (map fst formContents)) ++ "</div></form>"
		let callbacks = map snd formContents
		return (str, callbacks)

getConfigDataInfoForm :: JvHash -> ConfigDataInfo -> Fay (Text, Fay JQuery -> Fay ())
getConfigDataInfoForm configHash dataInfo = do
	case liftMaybe (processData (jvHashVal mName configHash)) (formEntryForType mType) of
		Nothing -> error $ "unknown member type " ++ mType ++ " or can't get data"
		Just result -> result
	where
		mType = memberType dataInfo
		mName = memberName dataInfo
		processData :: Maybe (Text, JValue) -> FormEntryProvider -> Fay (Text, Fay JQuery -> Fay())
		processData hashVal formEntry = do
			memberAsString <- case hashVal of
				Just _hv -> return $ jvGetString $ snd _hv
				Nothing -> return ""
			return (prvGetHtml formEntry mName memberAsString,
				prvCreateCallback formEntry mName)


getModalEnteredData :: [ConfigDataInfo] -> JQuery -> Fay [(Text,Text)]
getModalEnteredData types modal = sequence $ map (getConfigDataInfoFormValue modal) types 

getConfigDataInfoFormValue :: JQuery -> ConfigDataInfo -> Fay (Text, Text)
getConfigDataInfoFormValue modal dataInfo =
	case formEntryForType mType of
		Nothing -> error $ "unknown member type: " ++ mType
		Just formEntry -> prvGetValue formEntry modal mName >>= return . (,) mName
	where
		mType = memberType dataInfo
		mName = memberName dataInfo

bootstrapModal :: JQuery -> Fay ()
bootstrapModal = ffi "%1.modal('show')"

bootstrapModalHide :: JQuery -> Fay ()
bootstrapModalHide = ffi "%1.modal('hide')"

deleteConfigItemCb :: ConfigSection -> JValue -> Fay ()
deleteConfigItemCb section userSetting = do
	let pluginName = cfgPluginName $ pluginInfo section
	modal <- select "#myModal"
	findSelector "div.modal-header h4" modal >>= setText pluginName
	prepareModal (Danger "Delete") modal
	findSelector "div.modal-body" modal >>= setText "Are you sure you want to delete this data source?"
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

updatePluginConfig :: ConfigViewModel -> ConfigSection -> Text -> [(Text,Text)] -> JValue -> Fay ()
updatePluginConfig vm configSection pluginName newConfig oldConfig = do
	putStrLn $ "old config JS: " ++ (tshow oldConfig)
	let newConfigObj = jvArrayToObject newConfig
	let parm = jvGetString $ jqParam (tshow oldConfig)
	putStrLn $ "old config: " ++ parm
	ajxPut ("/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm) newConfigObj $ do
		ko_replaceElementObservableArray (userSettings configSection) oldConfig newConfigObj
		closePopup

addPluginConfig :: ConfigViewModel -> PluginConfig -> [(Text,Text)] -> Fay ()
addPluginConfig vm pluginConfig newConfig = do
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

deletePluginConfig :: ConfigViewModel -> PluginConfig -> JValue -> Event -> Fay ()
deletePluginConfig vm pluginConfig config _ = do
	let pluginName = cfgPluginName pluginConfig
	let parm = jvGetString $ jqParam (tshow config)
	let url = "/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm
	ajxDelete url closePopup

textReplaceAll :: Text -> Text -> Text -> Text
textReplaceAll = ffi "%3.split(%1).join(%2)"

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
