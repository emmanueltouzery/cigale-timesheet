{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards, RecursiveDo, TupleSections #-}

module Config where

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
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Ord

import Common

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

makeSimpleXhr :: (MonadWidget t m, FromJSON a) => String -> Event t b -> m (Dynamic t (RemoteData a))
makeSimpleXhr url postBuild = do
    req <- performRequestAsync $ const (xhrRequest "GET" url def) <$> postBuild
    holdDyn RemoteDataLoading $ fmap readRemoteData req

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

            renderDyn <- displayConfig =<< mapDyn fromRemoteData configDataDyn
            let configUpdateEvt = switch $ current renderDyn
        return ()

data ConfigUpdate = ConfigUpdate
    {
        oldConfigItemName :: String,
        newConfigItem :: ConfigItem
    } deriving Show
data ConfigDelete = ConfigDelete ConfigItem deriving Show
data ConfigChange = ChangeUpdate ConfigUpdate | ChangeDelete ConfigDelete deriving Show

applyConfigChange :: ConfigChange -> RemoteData FetchedData -> RemoteData FetchedData
applyConfigChange (ChangeUpdate (ConfigUpdate oldCiName newCi)) (RemoteData (FetchedData desc val)) =
    RemoteData (FetchedData desc updatedVal)
    where updatedVal = newCi : filter ((/= oldCiName) . configItemName) val
applyConfigChange (ChangeDelete (ConfigDelete ci)) (RemoteData (FetchedData desc val)) =
    RemoteData (FetchedData desc updatedVal)
    where updatedVal = filter (/= ci) val
applyConfigChange _ soFar = soFar

displayConfig :: MonadWidget t m => Dynamic t (Maybe FetchedData)
              -> m (Dynamic t (Event t ConfigChange))
displayConfig dynFetchedData = do
    rec
        cfgByProvider <- mapDyn (fromMaybe []) =<< mapDyn (liftA groupByProvider) dynFetchedData
        cfgChgEvt <- mapDyn leftmost =<< simpleList cfgByProvider displayConfigSection
    return cfgChgEvt

groupByProvider :: FetchedData -> [(PluginConfig, [ConfigItem])]
groupByProvider (FetchedData configDesc configVal) =
    fmap (second $ sortBy (comparing configItemName)) $ -- sort config items by name
    fmap (first fromJust) $ filter (isJust . fst) $     -- only keep Just providers.
    fmap (first $ providerByName configDesc) $          -- replace provider name by provider (Maybe)
    buckets providerName configVal                      -- bucket by provider name

providerByName :: [PluginConfig] -> String -> Maybe PluginConfig
providerByName pluginConfigs name = find ((== name) . cfgPluginName) pluginConfigs

type EditConfigItemRender t = (TextInput t, Map String (TextInput t))

editConfigItem :: MonadWidget t m => PluginConfig -> ConfigItem -> m (EditConfigItemRender t)
editConfigItem PluginConfig{..} ConfigItem{..} =
    el "form" $ do
        srcNameInput <- elAttr "fieldset" ("class" =: "form-group") $
            fieldEntry "sourceName" "Enter source name:" configItemName
        fieldInputs  <- Map.fromList <$> mapM (editConfigDataInfo configuration) cfgPluginConfig
        return (srcNameInput, fieldInputs)

editConfigDataInfo :: MonadWidget t m => A.Object -> ConfigDataInfo -> m (String, TextInput t)
editConfigDataInfo obj ConfigDataInfo{..} = do
    -- TODO different display based on member type: String, Text, ByteString, FilePath, FolderPath, Password
    let fieldValue = readObjectField memberName obj
    field <- case memberType of
        "Password" -> passwordEntry memberName memberName fieldValue
        _ -> fieldEntry memberName memberName fieldValue
    return (memberName, field)

fieldEntry :: MonadWidget t m => String -> String -> String -> m (TextInput t)
fieldEntry fieldId desc fieldValue = do
    elAttr "label" ("for" =: fieldId) $ text desc
    textInput $ def
        & textInputConfig_attributes .~ constDyn ("id" =: fieldId <> "class" =: "form-control")
        & textInputConfig_initialValue .~ fieldValue

-- TODO add button "show password"
passwordEntry :: MonadWidget t m => String -> String -> String -> m (TextInput t)
passwordEntry fieldId desc fieldValue = do
    elAttr "label" ("for" =: fieldId) $ text desc
    textInput $ def
        & textInputConfig_attributes .~ constDyn ("id" =: fieldId <> "class" =: "form-control")
        & textInputConfig_inputType .~ "password"
        & textInputConfig_initialValue .~ fieldValue

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
    rec
        dialogInfo <- buildModalDialog "Delete" (DangerBtn "Delete")
            (domEvent Click deleteBtn) (Just errorEvt)
            (text $ "Delete the config item " <> configItemName <> "?")

        let errorEvt = leftmost
                [
                    fmapMaybe remoteDataInvalidDesc saveEvt,
                    -- whenever the user opens the modal, clear the error display.
                    fmap (const "") $ domEvent Click deleteBtn
                ]

        let deleteEvt = fmap (const $ ConfigDelete ci) (okBtnEvent dialogInfo)
        saveEvt <- saveConfigDelete deleteEvt

    handleSaveAction dialogInfo saveEvt

addEditButton :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigUpdate)
addEditButton pluginConfig@PluginConfig{..} ci@ConfigItem{..} = do
    rec
        (editBtn, _) <- elAttr' "button" ("class" =: "btn btn-default btn-sm"
                                          <> "style" =: "float: right; margin-right: 5px") $ text "Edit"
        dialogInfo <- buildModalDialog "Edit" (PrimaryBtn "Save")
            (domEvent Click editBtn) (Just errorEvt) (editConfigItem pluginConfig ci)

        let errorEvt = leftmost
                [
                    fmapMaybe remoteDataInvalidDesc saveEvt,
                    -- whenever the user opens the modal, clear the error display.
                    fmap (const "") $ domEvent Click editBtn
                ]

        editConfigEvt <- performEvent $ fmap
            (const $ do
                  modalResult <- sample $ current $ bodyResult dialogInfo
                  readDialog modalResult pluginConfig configItemName)
            $ okBtnEvent dialogInfo
        saveEvt <- saveConfigEdit editConfigEvt
        cfgUpdEvt <- handleSaveAction dialogInfo saveEvt

    return cfgUpdEvt

handleSaveAction :: MonadWidget t m => ModalDialogResult t a -> Event t (RemoteData b) -> m (Event t b)
handleSaveAction dialogInfo saveEvt = do
    let savedCfgEditEvt = fmapMaybe fromRemoteData saveEvt
    -- request to close the dialog upon success
    performEvent_ $ fmap (const $ liftIO $ hideModalDialog dialogInfo) savedCfgEditEvt
    -- trigger the config update event to refresh the display
    -- when the dialog is actually closed, and if the save info is present.
    -- (when we refresh, the modal gets removed from the DOM
    -- and doesn't disappear properly, so refresh only after confirmed modal close)
    saveInfoDyn <- holdDyn Nothing $ fmap Just savedCfgEditEvt
    return $ fmapMaybe id $ tagDyn saveInfoDyn (closedEvent dialogInfo)

encodeToStr :: ToJSON a => a -> String
encodeToStr = bsToStr . encode
    where bsToStr = map (chr . fromEnum) . BS.unpack

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
              (TextInput t, Map String (TextInput t)) -> PluginConfig -> String
              -> m ConfigUpdate
readDialog (nameInput, cfgInputs) PluginConfig{..} oldConfigItemName = do
    newName <- sample $ current $ _textInput_value nameInput
    cfgList <- sequence $ flip map cfgPluginConfig $ \cfgDataInfo -> do
        let mName = memberName cfgDataInfo
        let inputField = fromJust $ Map.lookup mName cfgInputs
        val <- sample $ current $ _textInput_value inputField
        return (T.pack mName, A.String $ T.pack val)
    let configItem = ConfigItem newName cfgPluginName (HashMap.fromList cfgList)
    return $ ConfigUpdate oldConfigItemName configItem

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
