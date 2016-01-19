{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards, RecursiveDo, TupleSections #-}

module Config where

import GHCJS.DOM.HTMLInputElement

import Reflex.Dom

import Data.Aeson as A
import Data.Aeson.Types as A
import GHC.Generics
import Control.Applicative
import Data.List
import Data.Function
import Data.Maybe
import Data.Bifunctor
import Data.Monoid
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Ord

import Common
import FilePicker

-- TODO stop copy-pasting this between client & server

data PluginConfig = PluginConfig
    {
        cfgPluginName :: String,
        cfgPluginConfig :: [ConfigDataInfo]
    } deriving (Generic, Show, Eq)
instance FromJSON PluginConfig

data ConfigDataInfo = ConfigDataInfo
    {
        memberName :: String,
        memberType :: String
    } deriving (Eq, Show, Generic)
instance FromJSON ConfigDataInfo

data ConfigItem = ConfigItem
    {
        configItemName :: String,
        providerName :: String,
        configuration :: A.Object
    } deriving (Eq, Show, Generic)
instance FromJSON ConfigItem
instance ToJSON ConfigItem

data FetchedData = FetchedData
    {
        fetchedConfigDesc :: [PluginConfig],
        fetchedConfigVal  :: [ConfigItem]
    } deriving Show

configView :: MonadWidget t m => Dynamic t ActiveView -> m ()
configView activeViewDyn = do
    attrsDyn <- mapDyn (\curView -> styleWithHideIf (curView /= ActiveViewConfig)
                                    "height: 100%; padding-right: 10px;") activeViewDyn
    elDynAttr "div" attrsDyn $ do
        -- TODO getPostBuild ... maybe when the tab is loaded instead?
        postBuild <- getPostBuild
        cfgDescDyn <- makeSimpleXhr "/configdesc" postBuild
        cfgValDyn <- makeSimpleXhr "/configVal" postBuild
        readAllDyn <- combineDyn (liftA2 FetchedData) cfgDescDyn cfgValDyn

        rec
            configDataDyn <- foldDyn ($) RemoteDataLoading $ mergeWith (.)
                [
                    fmap const (updated readAllDyn),
                    fmap applyConfigChange configUpdateEvt
                ]

            configUpdateEvt <- displayConfig =<< mapDyn fromRemoteData configDataDyn
        return ()

data ConfigAdd = ConfigAdd ConfigItem deriving Show
data ConfigUpdate = ConfigUpdate
    {
        oldConfigItemName :: String,
        newConfigItem :: ConfigItem
    } deriving Show
data ConfigDelete = ConfigDelete ConfigItem deriving Show

data ConfigChange = ChangeAdd ConfigAdd
                  | ChangeUpdate ConfigUpdate
                  | ChangeDelete ConfigDelete
                  deriving Show

applyConfigChange :: ConfigChange -> RemoteData FetchedData -> RemoteData FetchedData
applyConfigChange (ChangeAdd (ConfigAdd newCi)) (RemoteData (FetchedData desc val)) =
    RemoteData (FetchedData desc (newCi:val))
applyConfigChange (ChangeUpdate (ConfigUpdate oldCiName newCi)) (RemoteData (FetchedData desc val)) =
    RemoteData (FetchedData desc updatedVal)
    where updatedVal = newCi : filter ((/= oldCiName) . configItemName) val
applyConfigChange (ChangeDelete (ConfigDelete ci)) (RemoteData (FetchedData desc val)) =
    RemoteData (FetchedData desc updatedVal)
    where updatedVal = filter (/= ci) val
applyConfigChange _ soFar = soFar

displayConfig :: MonadWidget t m => Dynamic t (Maybe FetchedData)
              -> m (Event t ConfigChange)
displayConfig dynFetchedData = do
    rec
        cfgByProvider <- mapDyn (fromMaybe []) =<< mapDyn (liftA groupByProvider) dynFetchedData
        let addCfgBtn = displayAddCfgButton . maybe [] fetchedConfigDesc
        addCfgEvt <- holdDyn never =<< (dyn =<< mapDyn addCfgBtn dynFetchedData)
        cfgChgEvt <- mapDyn leftmost =<< simpleList cfgByProvider displayConfigSection
    return $ leftmost $ fmap (switch . current) [addCfgEvt, cfgChgEvt]

displayAddCfgButton :: MonadWidget t m => [PluginConfig] -> m (Event t ConfigChange)
displayAddCfgButton pluginConfigs = do
    clickEvts <- elAttr "div" ("width" =: "100%"
                  <> "style" =: ("display: flex; flex-direction: row;"
                                 <> "justify-content: flex-end;"
                                 <> "padding-right: 30px; padding-bottom: 10px")) $
        elAttr "div" ("class" =: "btn-group") $ do
            elAttr "button" ("type" =: "button"
                             <> "class" =: "btn btn-primary dropdown-toggle"
                             <> "data-toggle" =: "dropdown"
                             <> "aria-haspopup" =: "true"
                             <> "aria-expanded" =: "false")
                $ text "Add..."
            elAttr "div" ("class" =: "dropdown-menu dropdown-menu-right") $
                mapM addCfgDropdownBtn pluginConfigs
    addEvts <- zipWithM addCfgPluginAdd pluginConfigs clickEvts
    return (ChangeAdd <$> leftmost addEvts)

addCfgDropdownBtn :: MonadWidget t m => PluginConfig -> m (Event t ())
addCfgDropdownBtn PluginConfig{..} = do
    (pcLnk, _) <- elAttr' "a" ("class" =: "dropdown-item"
                               <> "href" =: "javascript:void(0);") $
        text cfgPluginName
    return (domEvent Click pcLnk)

addCfgPluginAdd :: MonadWidget t m => PluginConfig -> Event t () -> m (Event t ConfigAdd)
addCfgPluginAdd pc clickEvt = do
    let ci = ConfigItem "" "" HashMap.empty
    modalDyn <- holdDyn pc $ fmap (const pc) clickEvt
    dynModalVal <- forDyn modalDyn $ \_ -> do
        rec
            (dialogResult, addDlgOkEvt, _) <- elAttr "div" ("style" =: "position: absolute") $
                buildModalBody' "Add" (PrimaryBtn "Save")
                    clickEvt errorEvt (editConfigItem pc ci)
            let errorEvt = fmapMaybe remoteDataInvalidDesc saveEvt

            editConfigEvt <- performEvent $ fmap
                (const $ do
                      ConfigAdd <$> readDialog dialogResult pc)
                addDlgOkEvt
            saveEvt <- saveConfigAdd editConfigEvt
        return saveEvt

    dynModalEvtEvt <- dynModal dynModalVal
    dynModalDynEvt <- holdDyn never dynModalEvtEvt
    modalHandleSaveAction $ switch $ current dynModalDynEvt

groupByProvider :: FetchedData -> [(PluginConfig, [ConfigItem])]
groupByProvider (FetchedData configDesc configVal) =
    fmap (second $ sortBy (comparing configItemName)) $ -- sort config items by name
    fmap (first fromJust) $ filter (isJust . fst) $     -- only keep Just providers.
    fmap (first $ providerByName configDesc) $          -- replace provider name by provider (Maybe)
    buckets providerName configVal                      -- bucket by provider name

providerByName :: [PluginConfig] -> String -> Maybe PluginConfig
providerByName pluginConfigs name = find ((== name) . cfgPluginName) pluginConfigs

type EditConfigItemRender t = (Dynamic t String, Map String (Dynamic t String))

editConfigItem :: MonadWidget t m => PluginConfig -> ConfigItem -> m (EditConfigItemRender t)
editConfigItem PluginConfig{..} ConfigItem{..} = do
    when (isJust $ find (== "Password") (memberType <$> cfgPluginConfig)) $
        elAttr "div" ("class" =: "alert alert-warning" <> "role" =: "alert") $
            el "strong" (text "Warning") >>
                text " passwords are stored in plain text in the configuration file!"
    el "form" $ do
        srcNameInput <- elAttr "fieldset" ("class" =: "form-group") $
            fieldEntry "sourceName" "Enter source name:" configItemName
        fieldInputs  <- Map.fromList <$> mapM (editConfigDataInfo configItemName configuration) cfgPluginConfig
        return (srcNameInput, fieldInputs)

editConfigDataInfo :: MonadWidget t m => String -> A.Object -> ConfigDataInfo -> m (String, Dynamic t String)
editConfigDataInfo cfgItemName obj ConfigDataInfo{..} = do
    -- TODO different display based on member type: String, Text, ByteString, FilePath, FolderPath, Password
    let fieldValue = readObjectField memberName obj
    field <- case memberType of
        "Password"   -> passwordEntry memberName memberName fieldValue
        "FolderPath" -> folderEntry cfgItemName memberName fieldValue
        _ -> fieldEntry memberName memberName fieldValue
    return (memberName, field)

folderEntry :: MonadWidget t m => String -> String -> String -> m (Dynamic t String)
folderEntry cfgItemName memberName val = do
    elAttr "label" ("for" =: memberName) $ text memberName
    elAttr "div" ("class" =: "input-group") $ do
        rec
            inputVal <- _textInput_value <$> textInput
                (def
                 & textInputConfig_attributes .~ constDyn ("id" =: memberName <> "class" =: "form-control")
                 & textInputConfig_initialValue .~ val)
            (browseBtn, _) <- elAttr' "div" ("class" =: "input-group-addon") $
                elAttr' "span" ("style" =: "cursor: pointer") $ text "Browse..."
            buildFolderPicker (domEvent Click browseBtn)
            return inputVal
        --let inputGetValue = htmlInputElementGetValue . castToHTMLInputElement . _el_element
        return inputVal

fieldEntry :: MonadWidget t m => String -> String -> String -> m (Dynamic t String)
fieldEntry fieldId desc fieldValue = do
    elAttr "label" ("for" =: fieldId) $ text desc
    _textInput_value <$> textInput
        (def
         & textInputConfig_attributes .~ constDyn ("id" =: fieldId <> "class" =: "form-control")
         & textInputConfig_initialValue .~ fieldValue)

passwordEntry :: MonadWidget t m => String -> String -> String -> m (Dynamic t String)
passwordEntry fieldId desc fieldValue = do
    elAttr "label" ("for" =: fieldId) $ text desc
    elAttr "div" ("class" =: "input-group") $ do
        rec
            attrsDyn <- forDyn showPaswd
                (\p -> "class" =: "form-control"
                       <> "id" =: fieldId
                       <> "value" =: fieldValue
                       <> "type" =: if p then "password" else "text")
            (inputField, _) <- elDynAttr' "input" attrsDyn $ return ()
            showPaswd <- foldDyn ($) True $ leftmost [fmap (const not) padlockEvt]
            (padlock, _) <- elAttr' "div" ("class" =: "input-group-addon") $ do
                padlockContents <- forDyn showPaswd (\case; True -> "&#128274;"; False -> "&#128275;")
                elDynHtmlAttr' "span" ("style" =: "cursor: pointer") padlockContents
            let padlockEvt = domEvent Click padlock
        let inputGetValue = htmlInputElementGetValue . castToHTMLInputElement . _el_element
        holdDyn fieldValue =<< performEvent
            (fmap (const $ liftIO $ inputGetValue inputField) $ domEvent Change inputField)

buckets :: Ord b => (a -> b) -> [a] -> [(b, [a])]
buckets f = map (\g -> (fst $ head g, map snd g))
          . groupBy ((==) `on` fst)
          . sortBy (comparing fst)
          . map (\x -> (f x, x))

displayConfigSection :: MonadWidget t m => Dynamic t (PluginConfig, [ConfigItem])
                     -> m (Event t ConfigChange)
displayConfigSection dynSecInfo_ = do
    let dynSecInfo = nubDyn dynSecInfo_
    dynPluginConfig <- mapDyn fst dynSecInfo
    dynConfigItems  <- mapDyn snd dynSecInfo
    elAttr "div" ("class" =: "card") $ do
        elAttr "h5" ("class" =: "card-header") $ dynText =<< mapDyn cfgPluginName dynPluginConfig
        elAttr "div" ("class" =: "card-block") $ do
            dynParams <- combineDyn (\cfg dataInfos -> map (cfg,) dataInfos) dynPluginConfig dynConfigItems
            dynEvtsAr <- mapDyn (sequence . fmap (uncurry displaySectionItem)) dynParams
            dynEvts <- fmap leftmost <$> dyn dynEvtsAr
            (switch . current) <$> holdDyn never dynEvts

displaySectionItem :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigChange)
displaySectionItem pluginConfig@PluginConfig{..} ci@ConfigItem{..} =
    elAttr "div" ("class" =: "card") $ do
        cfgChgEvt <- elAttr "div" ("class" =: "card-header") $ do
            text configItemName
            delEvt <- addDeleteButton ci
            updEvt <- addEditButton pluginConfig ci
            -- leftmost is ok, they can't both happen at the same time
            return $ leftmost [fmap ChangeDelete delEvt, fmap ChangeUpdate updEvt]
        elAttr "div" ("class" =: "card-block") $
            pluginContents pluginConfig ci
        return cfgChgEvt

addDeleteButton :: MonadWidget t m => ConfigItem -> m (Event t ConfigDelete)
addDeleteButton ci@ConfigItem{..} = do
    (deleteBtn, _) <- elAttr' "button"
        ("class" =: "btn btn-danger btn-sm"
         <> "style" =: "float: right") $ text "Delete"
    modalDyn <- holdDyn ci $ fmap (const ci) (domEvent Click deleteBtn)
    dynModalVal <- forDyn modalDyn $ \_ -> do
        rec
            (_, deleteDlgOkEvt, _) <- buildModalBody' "Delete" (DangerBtn "Delete")
                (domEvent Click deleteBtn) errorEvt
                (text $ "Delete the config item " <> configItemName <> "?")
            let errorEvt = fmapMaybe remoteDataInvalidDesc saveEvt

            let deleteEvt = fmap (const $ ConfigDelete ci) deleteDlgOkEvt
            saveEvt <- saveConfigDelete deleteEvt
        return saveEvt

    dynModalEvtEvt <- dynModal dynModalVal
    dynModalDynEvt <- holdDyn never dynModalEvtEvt
    modalHandleSaveAction $ switch $ current dynModalDynEvt

addEditButton :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigUpdate)
addEditButton pluginConfig@PluginConfig{..} ci@ConfigItem{..} = do
    (editBtn, _) <- elAttr' "button" ("class" =: "btn btn-default btn-sm"
                                      <> "style" =: "float: right; margin-right: 5px") $ text "Edit"
    modalDyn <- holdDyn ci $ fmap (const ci) (domEvent Click editBtn)
    dynModalVal <- forDyn modalDyn $ \_ -> do
        rec
            (dialogResult, editDlgOkEvt, _) <- buildModalBody' "Edit" (PrimaryBtn "Save")
                (domEvent Click editBtn) errorEvt (editConfigItem pluginConfig ci)
            let errorEvt = fmapMaybe remoteDataInvalidDesc saveEvt

            editConfigEvt <- performEvent $ fmap
                (const $ ConfigUpdate configItemName <$> readDialog dialogResult pluginConfig)
                editDlgOkEvt
            saveEvt <- saveConfigEdit editConfigEvt
        return saveEvt

    dynModalEvtEvt <- dynModal dynModalVal
    dynModalDynEvt <- holdDyn never dynModalEvtEvt
    modalHandleSaveAction $ switch $ current dynModalDynEvt

modalHandleSaveAction :: MonadWidget t m => Event t (RemoteData b) -> m (Event t b)
modalHandleSaveAction saveEvt = do
    let savedCfgEditEvt = fmapMaybe fromRemoteData saveEvt
    -- request to close the dialog upon success
    performEvent_ $ fmap (const $ liftIO $ hideModalIdDialog topLevelModalId) savedCfgEditEvt
    return savedCfgEditEvt

encodeToStr :: ToJSON a => a -> String
encodeToStr = bsToStr . encode
    where bsToStr = map (chr . fromEnum) . BS.unpack

saveConfigAdd :: MonadWidget t m => Event t ConfigAdd -> m (Event t (RemoteData ConfigAdd))
saveConfigAdd configAddEvt = do
    let makeReq (ConfigAdd cfg) = do
            let url = "/config"
            xhrRequest "POST" url $
                def { _xhrRequestConfig_sendData = Just (encodeToStr cfg) }
    httpVoidRequest makeReq configAddEvt

saveConfigEdit :: MonadWidget t m => Event t ConfigUpdate -> m (Event t (RemoteData ConfigUpdate))
saveConfigEdit configEditEvt = do
    let makeReq cfgEdit = do
            let url = "/config?oldConfigItemName=" <> oldConfigItemName cfgEdit
            xhrRequest "PUT" url $
                def { _xhrRequestConfig_sendData = Just (encodeToStr $ newConfigItem cfgEdit) }
    httpVoidRequest makeReq configEditEvt

saveConfigDelete :: MonadWidget t m => Event t ConfigDelete -> m (Event t (RemoteData ConfigDelete))
saveConfigDelete cfgDelEvt = do
    let makeReq (ConfigDelete cfg) = do
            let url = "/config?configItemName=" <> configItemName cfg
            xhrRequest "DELETE" url def
    httpVoidRequest makeReq cfgDelEvt

httpVoidRequest :: MonadWidget t m => (a -> XhrRequest) -> Event t a -> m (Event t (RemoteData a))
httpVoidRequest makeReq evt = do
    -- take advantage of the Traversable instance for pairs, which
    -- will apply the function to the second element only, to pass
    -- on the ConfigUpdate besides the XhrResponse in the resulting event.
    let reqEvt = fmap (\x -> (x, makeReq x)) evt
    resp <- performRequestsAsync reqEvt
    return $ fmap (\(cu, rsp) -> const cu <$> readEmptyRemoteData rsp) resp

readDialog :: MonadSample t m =>
              EditConfigItemRender t -> PluginConfig
              -> m ConfigItem
readDialog (nameInput, cfgInputs) PluginConfig{..} = do
    newName <- sample $ current nameInput
    cfgList <- sequence $ flip map cfgPluginConfig $ \cfgDataInfo -> do
        let mName = memberName cfgDataInfo
        let inputField = fromJust $ Map.lookup mName cfgInputs
        val <- sample $ current inputField
        return (T.pack mName, A.String $ T.pack val)
    return $ ConfigItem newName cfgPluginName (HashMap.fromList cfgList)

readObjectField :: String -> A.Object -> String
readObjectField fieldName aObject = fromMaybe "" (A.parseMaybe (\obj -> obj .: T.pack fieldName) aObject)

pluginContents :: MonadWidget t m => PluginConfig -> ConfigItem -> m ()
pluginContents pluginConfig configContents = elAttr "table" ("class" =: "table") $
    mapM_ (getPluginElement $ configuration configContents) (cfgPluginConfig pluginConfig)

getPluginElement :: MonadWidget t m => A.Object -> ConfigDataInfo -> m ()
getPluginElement config ConfigDataInfo{..} = do
    let memberValue = readObjectField memberName config
    let memberValueDisplay = case memberType of
            "Password" -> replicate (length memberValue) '*'
            _          -> memberValue
    el "tr" $ do
        el "td" $ text memberName
        el "td" $ text memberValueDisplay
