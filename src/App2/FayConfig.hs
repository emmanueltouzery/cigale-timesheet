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
			forM_ configArray (addPlugin (cfgPluginConfig myP) header)

addModuleMenuItem :: PluginConfig -> Fay JQuery
addModuleMenuItem pluginConfig = do
	let pluginName = cfgPluginName pluginConfig
	parent <- select "ul#add_module_menu"
	menuItem <- (select $ "<li><a href='#'>" ++ pluginName ++ "</a></li>") >>= appendTo parent
	findSelector "a" menuItem >>= click (addModuleAction pluginName pluginConfig)

addModuleAction :: String -> PluginConfig -> Event -> Fay ()
addModuleAction pluginName pluginConfig _ = do
	modal <- select "#myModal"
	findSelector "div.modal-header h4" modal >>= setText pluginName
	--findSelector "div.modal-body" modal >>= setText (getModalContents pluginConfig)
	bootstrapModal modal

getModalContents :: PluginConfig -> String
getModalContents pluginConfig = ""

bootstrapModal :: JQuery -> Fay ()
bootstrapModal = ffi "%1.modal('show')"

bootstrapPanel :: JQuery -> String -> Fay JQuery
bootstrapPanel parent title = do
	let panelHtml = "<div class='panel panel-default'><div class='panel-heading'>" ++
		"<h3 class='panel-title'>" ++ title ++ "</h3></div><div class='panel-body'>"
		++ "</div></div>"
	panelRoot <- (select panelHtml) >>= appendTo parent
	findSelector "div.panel-body" panelRoot

bootstrapWell :: JQuery -> Fay JQuery
bootstrapWell parent = (select "<div class='well well-sm'></div>") >>= appendTo parent

addPlugin :: [ConfigDataType] -> JQuery -> JValue -> Fay ()
addPlugin configDataTypes header config = forM_ configDataTypes (addParameter header config)

addParameter :: JQuery -> JValue -> ConfigDataType -> Fay ()
addParameter header config dataType = do
	parameterWell <- bootstrapWell header
	forM_ (members dataType) (addPluginElement parameterWell config)

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
