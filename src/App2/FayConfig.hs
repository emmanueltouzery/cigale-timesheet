{-# LANGUAGE NoImplicitPrelude, EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}

import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import JQuery
import Prelude hiding ((++), error, putStrLn)
import qualified Prelude as P

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
main = ready refreshDisplay

refreshDisplay :: Fay ()
refreshDisplay = myajax2 "/configVal" "/configdesc" $ \val desc -> handleValDesc (head val) (head desc)

handleValDesc :: JValue -> [PluginConfig] -> Fay ()
handleValDesc configVal pluginConfig = do
	divCurConfig <- select "div#curConfig" >>= setHtml ""
	let hash = jvAsHash configVal
	-- TODO probably need a proper Text comparison...
	let sortedHash = sortBy (\(a, _) (b, _) -> strComp (T.unpack a) (T.unpack b)) hash
	-- display the user's current config
	forM_ sortedHash (displayPluginConfig pluginConfig divCurConfig)
	-- offer to add new modules
	forM_ pluginConfig addModuleMenuItem

displayPluginConfig :: [PluginConfig] -> JQuery -> (Text, JValue) -> Fay ()
displayPluginConfig pluginConfig divCurConfig (pluginName, config) = do
	putStrLn pluginName
	let myPluginConfig = find (\x -> cfgPluginName x == pluginName) pluginConfig
	header <- bootstrapPanel divCurConfig pluginName
	let configArray = jvArray config
	case myPluginConfig of
		Nothing -> putStrLn $ "can't find config info for " ++ pluginName
		Just myP -> forM_ configArray (addPlugin myP header)

addModuleMenuItem :: PluginConfig -> Fay JQuery
addModuleMenuItem pluginConfig = do
	let pluginName = cfgPluginName pluginConfig
	parent <- select "ul#add_module_menu"
	menuItem <- (select $ "<li><a href='#'>" ++ pluginName ++ "</a></li>") >>= appendTo parent
	findSelector "a" menuItem >>= click (\_ -> addEditModuleAction pluginConfig Nothing)

addEditModuleAction :: PluginConfig -> Maybe JValue -> Fay ()
addEditModuleAction pluginConfig mbExistingConfig = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	findSelector "div.modal-header h4" modal >>= setText pluginName
	prepareModal (Primary "Save changes") modal
	let configMembers = cfgPluginConfig pluginConfig
	(modalContents, callbacks) <- getModalContents configMembers mbExistingConfig
	let contentsSelector = findSelector "div.modal-body" modal
	contentsSelector >>= setHtml modalContents
	putStrLn "running callbacks"
	mapM_ ($ contentsSelector) callbacks
	putStrLn "ran callbacks"
	let clickCallback enteredData = case mbExistingConfig of
		Nothing -> addPluginConfig pluginName enteredData
		Just existingConfig -> do
			updatePluginConfig pluginName enteredData existingConfig
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

bootstrapPanel :: JQuery -> Text -> Fay JQuery
bootstrapPanel parent title = do
	let panelHtml = "<div class='panel panel-default'><div class='panel-heading'>" ++
		"<h3 class='panel-title'>" ++ title ++ "</h3></div><div class='panel-body'>"
		++ "</div></div>"
	panelRoot <- (select panelHtml) >>= appendTo parent
	findSelector "div.panel-body" panelRoot

bootstrapButton :: Text -> Text
bootstrapButton name = "<button type='button' class='btn btn-default btn-lg' id='" ++ name ++ "'>"
		++ "<span class='glyphicon glyphicon-" ++ name ++ "'></span></button>"

-- putting XS for the columns because i want a responsive design,
-- that it behaves well when the user reduces the width of the browser
bootstrapWell :: JQuery -> Fay JQuery
bootstrapWell parent = (select $ "<div class='well well-sm'><div class='row'>"
		++ "<div class='col-xs-8' id='contents'>"
		++ "</div><div class='col-xs-4'><div class='btn-toolbar'"
		++ " style='float: right;'>" ++ (bootstrapButton "edit")
		++ (bootstrapButton "remove-circle") ++ "</div></div></div>") >>= appendTo parent

addPlugin :: PluginConfig -> JQuery -> JValue -> Fay ()
addPlugin pluginConfig header config = do
	parameterWell <- bootstrapWell header
	parameterContentsDiv <- findSelector "div#contents" parameterWell
	let configDataInfos = cfgPluginConfig pluginConfig
	forM_ configDataInfos (addPluginElement parameterContentsDiv config)
	findSelector "#edit" parameterWell >>= click (\_ -> addEditModuleAction pluginConfig (Just config))
	findSelector "#remove-circle" parameterWell >>= click (\_ -> deleteModuleAction pluginConfig config)
	return ()

deleteModuleAction :: PluginConfig -> JValue -> Fay ()
deleteModuleAction pluginConfig config = do
	let pluginName = cfgPluginName pluginConfig
	modal <- select "#myModal"
	findSelector "div.modal-header h4" modal >>= setText pluginName
	prepareModal (Danger "Delete") modal
	findSelector "div.modal-body" modal >>= setText "Are you sure you want to delete this data source?"
	bootstrapModal modal
	findSelector "button#main-action" modal >>= click (deletePluginConfig pluginConfig config)
	return ()

getOriginalPluginConfig :: JValue -> [(Text,Text)]
getOriginalPluginConfig json = map toStrPair (jvAsHash json)
	where
		toStrPair (a, b) = (a, jvGetString b)

updatePluginConfig :: Text -> [(Text,Text)] -> JValue -> Fay ()
updatePluginConfig pluginName newConfig oldConfig = do
	putStrLn $ "old config JS: " ++ (tshow oldConfig)
	let newConfigObj = jvArrayToObject newConfig
	let parm = jvGetString $ jqParam (tshow oldConfig)
	putStrLn $ "old config: " ++ parm
	ajxPut ("/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm) newConfigObj closePopupAndRefresh

addPluginConfig :: Text -> [(Text,Text)] -> Fay ()
addPluginConfig pluginName newConfig = do
	let newConfigObj = jvArrayToObject newConfig
	ajxPost ("/config?pluginName=" ++ pluginName) newConfigObj closePopupAndRefresh 

closePopupAndRefresh :: Fay ()
closePopupAndRefresh = do
	select "#myModal" >>= bootstrapModalHide
	refreshDisplay

deletePluginConfig :: PluginConfig -> JValue -> Event -> Fay ()
deletePluginConfig pluginConfig config _ = do
	let pluginName = cfgPluginName pluginConfig
	let parm = jvGetString $ jqParam (tshow config)
	let url = "/config?pluginName=" ++ pluginName ++ "&oldVal=" ++ parm
	ajxDelete url closePopupAndRefresh

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

addPluginElement :: JQuery -> JValue -> ConfigDataInfo -> Fay ()
addPluginElement header config dataInfo = do
	let memberNameV = memberName dataInfo
	let memberValue = jvGetString (jvValue config memberNameV)
	let memberValueDisplay = case (memberType dataInfo) of
		"Password" -> T.pack $ replicate (T.length memberValue) '*'
		_ -> memberValue
	(select $ "<div>" ++ memberNameV ++ " " ++ memberValueDisplay ++ "</div>") >>= appendTo header
	return ()

-- http://stackoverflow.com/questions/18025474/multiple-ajax-queries-in-parrallel-with-fay
myajax2 :: Text -> Text -> (Automatic b -> Automatic c -> Fay ()) -> Fay ()
myajax2 = ffi "$.when($.getJSON(%1), $.getJSON(%2)).then(%3)"
