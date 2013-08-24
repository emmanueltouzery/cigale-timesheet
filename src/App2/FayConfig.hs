{-# LANGUAGE NoImplicitPrelude, EmptyDataDecls #-}

import Prelude
import FFI
import JQuery

data JValue

jvKeys :: JValue -> Fay [String]
jvKeys = ffi "getKeys(%1)"

jvValue :: JValue -> String -> Fay JValue
jvValue = ffi "%1[%2]"

jvArray :: JValue -> Fay [JValue]
jvArray = ffi "%1"

jvAsHash :: JValue -> Fay [(String, JValue)]
jvAsHash value = do
	keys <- jvKeys value
	values <- sequence $ map (jvValue value) keys
	return $ zip keys values

jvGetString :: JValue -> Fay String
jvGetString = ffi "%1"

data ConfigDataInfo = ConfigDataInfo
	{
		memberName :: String,
		memberType :: String
	} deriving (Eq, Show)

data ConfigDataType = ConfigDataType
	{
		dataName :: String,
		members :: [ConfigDataInfo]
	} deriving (Eq, Show)

data PluginConfig = PluginConfig
	{
		cfgPluginName :: String,
		cfgPluginConfig :: [ConfigDataType]
	}

main :: Fay ()
main = ready $ myajax2 "/configVal" "/configdesc" $ \val desc -> handleValDesc (head val) (head desc)

handleValDesc :: JValue -> [PluginConfig] -> Fay ()
handleValDesc configVal pluginConfig = do
	divCurConfig <- select "div#curConfig"
	hash <- jvAsHash configVal
	-- display the user's current config
	forM_ hash (displayPluginConfig pluginConfig divCurConfig)
	-- offer to add new modules
	forM_ pluginConfig addModuleMenuItem

displayPluginConfig :: [PluginConfig] -> JQuery -> (String, JValue) -> Fay ()
displayPluginConfig pluginConfig divCurConfig (pluginName, config) = do
	putStrLn pluginName
	let myPluginConfig = find (\x -> cfgPluginName x == pluginName) pluginConfig
	header <- bootstrapPanel divCurConfig pluginName
	configArray <- jvArray config
	case myPluginConfig of
		Nothing -> putStrLn $ "can't find config info for " ++ pluginName
		Just myP -> do
			forM_ configArray (addPlugin myP header)

addModuleMenuItem :: PluginConfig -> Fay JQuery
addModuleMenuItem pluginConfig = do
	let pluginName = cfgPluginName pluginConfig
	parent <- select "ul#add_module_menu"
	menuItem <- (select $ "<li><a href='#'>" ++ pluginName ++ "</a></li>") >>= appendTo parent
	findSelector "a" menuItem >>= click (\_ -> addModuleAction pluginConfig)

addModuleAction :: PluginConfig -> Fay ()
addModuleAction pluginConfig = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	findSelector "div.modal-header h4" modal >>= setText pluginName
	prepareModal (Primary "Save changes") modal
	--let configMembers = members $ cfgPluginConfig pluginConfig
	--findSelector "div.modal-body" modal >>= setText (getModalContents configMembers)
	bootstrapModal modal

data MainAction = Primary String
		  | Danger String
	deriving (Show)

prepareModal :: MainAction -> JQuery -> Fay JQuery
prepareModal action modal = do
	footer <- findSelector "div.modal-footer" modal
	findSelector "#main-action" footer >>= remove
	let btnHtml = "<button type='button' id='main-action' class='btn btn-" ++ btnType ++ "'>" ++ actionText ++ "</button>"
	(select btnHtml) >>= appendTo footer
	where
		(btnType, actionText) = case action of
			Primary x -> ("primary", x)
			Danger x -> ("danger", x)
			_ -> error $ "Unknown action: " ++ (show action)

getModalContents :: [ConfigDataInfo] -> String
getModalContents types = "<form role='form'><div class='form-group'>"
		++ formContents ++ "</div></form>"
	where formContents = concatMap getConfigDataInfoForm types

getConfigDataInfoForm :: ConfigDataInfo -> String
getConfigDataInfoForm dataInfo = case memberType dataInfo of
	"String" -> addTextEntry $ memberName dataInfo
	_ -> error $ "unknown member type: " ++ memberType dataInfo

addTextEntry :: String -> String
addTextEntry memberName = "<label for='{}'>{}</label><input type='text'"
				++ " class='form-control' id='{}' placeholder='Enter {}'></div>"

bootstrapModal :: JQuery -> Fay ()
bootstrapModal = ffi "%1.modal('show')"

bootstrapPanel :: JQuery -> String -> Fay JQuery
bootstrapPanel parent title = do
	let panelHtml = "<div class='panel panel-default'><div class='panel-heading'>" ++
		"<h3 class='panel-title'>" ++ title ++ "</h3></div><div class='panel-body'>"
		++ "</div></div>"
	panelRoot <- (select panelHtml) >>= appendTo parent
	findSelector "div.panel-body" panelRoot

bootstrapButton :: String -> String
bootstrapButton name = "<button type='button' class='btn btn-default btn-lg' id='" ++ name ++ "'>"
		++ "<span class='glyphicon glyphicon-" ++ name ++ "'></span></button>"

bootstrapWell :: JQuery -> Fay JQuery
bootstrapWell parent = (select $ "<div class='well well-sm'>"
		++ "<div class='btn-toolbar' style='float: right;'>" ++ (bootstrapButton "edit")
		++ (bootstrapButton "remove-circle") ++ "</div></div>") >>= appendTo parent

addPlugin :: PluginConfig -> JQuery -> JValue -> Fay ()
addPlugin pluginConfig header config = do
	let configDataTypes = cfgPluginConfig pluginConfig
	forM_ configDataTypes (addParameter pluginConfig header config)

addParameter :: PluginConfig -> JQuery -> JValue -> ConfigDataType -> Fay ()
addParameter pluginConfig header config dataType = do
	parameterWell <- bootstrapWell header
	forM_ (members dataType) (addPluginElement parameterWell config)
	findSelector "#edit" parameterWell >>= click (\_ -> editConfigItem pluginConfig config dataType)
	findSelector "#remove-circle" parameterWell >>= click (\_ -> deleteConfigItem pluginConfig config dataType)
	return ()

editConfigItem :: PluginConfig -> JValue -> ConfigDataType -> Fay ()
editConfigItem pluginConfig config dataType = addModuleAction pluginConfig

deleteConfigItem :: PluginConfig -> JValue -> ConfigDataType -> Fay ()
deleteConfigItem pluginConfig config dataType = deleteModuleAction pluginConfig

deleteModuleAction :: PluginConfig -> Fay ()
deleteModuleAction pluginConfig = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	findSelector "div.modal-header h4" modal >>= setText pluginName
	prepareModal (Danger "Delete") modal
	--let configMembers = members $ cfgPluginConfig pluginConfig
	--findSelector "div.modal-body" modal >>= setText (getModalContents configMembers)
	bootstrapModal modal

addPluginElement :: JQuery -> JValue -> ConfigDataInfo -> Fay ()
addPluginElement header config dataInfo = do
	let memberNameV = memberName dataInfo
	putStrLn memberNameV
	memberValue <- (jvValue config memberNameV) >>= jvGetString
	(select $ "<div>" ++ memberNameV ++ " " ++ memberValue ++ "</div>") >>= appendTo header
	return ()

-- http://stackoverflow.com/questions/18025474/multiple-ajax-queries-in-parrallel-with-fay
myajax2 :: String -> String -> (Automatic b -> Automatic c -> Fay ()) -> Fay ()
myajax2 = ffi "$.when($.getJSON(%1), $.getJSON(%2)).then(%3)"
