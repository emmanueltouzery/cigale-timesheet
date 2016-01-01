{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards #-}

module Config where

import GHCJS.DOM.HTMLInputElement
import GHCJS.Foreign

import Reflex.Dom
import Reflex.Host.Class

import Data.Aeson as A
import Data.Aeson.Types as A
import GHC.Generics
import Control.Applicative
import Control.Monad
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
import Data.IORef

import Common

-- TODO stop copy-pasting this between client & server

data PluginConfig = PluginConfig
    {
        cfgPluginName :: String,
        cfgPluginConfig :: [ConfigDataInfo]
    } deriving Generic
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
    } deriving (Show, Generic)
instance FromJSON ConfigItem
instance ToJSON ConfigItem

data FetchedData = FetchedData
    {
        fetchedConfigDesc :: [PluginConfig],
        fetchedConfigVal  :: [ConfigItem]
    }

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
        void $ mapDyn displayConfig readAllDyn >>= dyn

displayConfig :: MonadWidget t m => RemoteData FetchedData -> m ()
displayConfig RemoteDataLoading = return ()
displayConfig (RemoteDataInvalid msg) = text msg
displayConfig (RemoteData (FetchedData configDesc configVal)) = do
    -- add all providers editing modal dialog to add or edit.
    -- they are retrieved through their DOM ID, the provider name.
    modalDialogInfos <- Map.fromList <$> mapM addProviderDialog configDesc
    let cfgByProvider =
            fmap (first fromJust) $ filter (isJust . fst) $  -- only keep Just providers.
            fmap (first $ providerByName configDesc) $       -- replace provider name by provider (Maybe)
            buckets providerName configVal                   -- bucket by provider name
    mapM_ (\(pluginConfig, configItems) ->
            displayConfigSection pluginConfig configItems
            (fromJust $ Map.lookup (cfgPluginName pluginConfig) modalDialogInfos)) cfgByProvider

providerByName :: [PluginConfig] -> String -> Maybe PluginConfig
providerByName pluginConfigs name = find ((== name) . cfgPluginName) pluginConfigs

data ProviderDialogInfo t = ProviderDialogInfo
     {
         pdProviderName :: String,
         pdSourceNameEntry :: TextInput t,
         pdTextEntries :: Map String (TextInput t),
         pdOkEvent :: Event t (),
         pdCloseEvent :: Event t (),
         pdErrorTrigger :: IORef (Maybe (EventTrigger t String))
     }

addProviderDialog :: MonadWidget t m => PluginConfig -> m (String, ProviderDialogInfo t)
addProviderDialog pluginConfig@PluginConfig{..} = do
     (errorDisplayEvt, errorDisplayEvtTrigger) <- newEventWithTriggerRef
     modalResult <- buildModalDialog cfgPluginName "Edit" "Save" (editConfigItem pluginConfig) (Just errorDisplayEvt)
     let (srcNameInput, fieldInputs) = bodyResult modalResult
     return (cfgPluginName, ProviderDialogInfo cfgPluginName srcNameInput fieldInputs (okEvent modalResult) (closeEvent modalResult) errorDisplayEvtTrigger)

editConfigItem :: MonadWidget t m => PluginConfig -> m (TextInput t, Map String (TextInput t))
editConfigItem PluginConfig{..} =
    el "form" $ do
        srcNameInput <- elAttr "fieldset" ("class" =: "form-group") $ fieldEntry "sourceName" "Enter source name:"
        fieldInputs  <- Map.fromList <$> mapM editConfigDataInfo cfgPluginConfig
        return (srcNameInput, fieldInputs)

editConfigDataInfo :: MonadWidget t m => ConfigDataInfo -> m (String, TextInput t)
editConfigDataInfo ConfigDataInfo{..} = do
    -- TODO different display based on member type: String, Text, ByteString, FilePath, FolderPath, Password
    field <- fieldEntry memberName memberName
    return (memberName, field)

fieldEntry :: MonadWidget t m => String -> String -> m (TextInput t)
fieldEntry fieldId desc = do
    elAttr "label" ("for" =: fieldId) $ text desc
    textInput $ def
        & textInputConfig_attributes .~ constDyn ("id" =: fieldId <> "class" =: "form-control")

buckets :: Ord b => (a -> b) -> [a] -> [(b, [a])]
buckets f = map (\g -> (fst $ head g, map snd g))
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))

displayConfigSection :: MonadWidget t m => PluginConfig -> [ConfigItem] -> ProviderDialogInfo t -> m ()
displayConfigSection secInfo secItems dialogInfo =
    void $ elAttr "div" ("class" =: "card") $ do
        elAttr "h5" ("class" =: "card-header") $ text $ cfgPluginName secInfo
        elAttr "div" ("class" =: "card-block") $
            mapM_ (displaySectionItem secInfo dialogInfo) secItems

displaySectionItem :: MonadWidget t m => PluginConfig -> ProviderDialogInfo t -> ConfigItem -> m ()
displaySectionItem pluginConfig@PluginConfig{..} dialogInfo ci@ConfigItem{..} =
    elAttr "div" ("class" =: "card") $ do
        elAttr "div" ("class" =: "card-header") $ do
            postGui <- askPostGui
            runWithActions <- askRunWithActions
            text configItemName
            (editBtn, _) <- elAttr' "button" ("class" =: "btn btn-default btn-sm"
                                              <> "style" =: "float: right"
                                              <> "data-toggle" =: "modal"
                                              <> "data-target" =: ("#" <> cfgPluginName)) $ text "Edit"
            performEvent_ $ fmap
                (const $ liftIO $ do
                      -- remove error displays
                      postGui $ handleTrigger runWithActions "" (pdErrorTrigger dialogInfo)
                      -- fill the dialog
                      fillDialog dialogInfo ci) $
                domEvent Click editBtn
            -- to save, i give to the server the old name and the new full ConfigItem.
            -- listen to the OK & close events. stop listening to OK on close (using gate)
            let popupOpenCloseEvent = leftmost
                    [
                        fmap (const True) $ domEvent Click editBtn,
                        fmap (const False) $ pdCloseEvent dialogInfo
                    ]
            isDisplayed <- holdDyn False popupOpenCloseEvent

            -- need IO to read the contents of the modal => build a new event
            -- that I populate reading using IO.
            (editConfigEvt, editConfigEvtTrigger) <- newEventWithTriggerRef
            performEvent_ $ fmap
                (const $ liftIO $ do
                      dlgInfo <- readDialog dialogInfo pluginConfig
                      postGui (handleTrigger runWithActions (configItemName, dlgInfo) editConfigEvtTrigger))
                $ gate (current isDisplayed) (pdOkEvent dialogInfo)
            saveEvt <- saveConfigItem editConfigEvt
            let handleEditResponse = \case
                    RemoteDataLoading -> return ()
                    RemoteDataInvalid msg -> postGui $ handleTrigger runWithActions msg (pdErrorTrigger dialogInfo)
                    (RemoteData x) -> liftIO $ hideModalDialog $ toJSString cfgPluginName -- TODO refresh display
            performEvent_ $ fmap (liftIO . handleEditResponse) saveEvt
        elAttr "div" ("class" =: "card-block") $
            pluginContents pluginConfig ci

byteStringToString :: BS.ByteString -> String
byteStringToString = map (chr . fromEnum) . BS.unpack

saveConfigItem :: MonadWidget t m => Event t (String, ConfigItem) -> m (Event t (RemoteData ()))
saveConfigItem configEditEvent = do
    let reqEvt = fmap buildXhrRequest configEditEvent
    req <- performRequestAsync reqEvt
    return $ fmap readEmptyRemoteData req

buildXhrRequest :: (String, ConfigItem) -> XhrRequest
buildXhrRequest (oldConfigItemName, newConfigItem) =
    xhrRequest "PUT" url $ def { _xhrRequestConfig_sendData = xhrData }
    where
      xhrData = Just (byteStringToString $ encode newConfigItem)
      url = "/config?oldConfigItemName=" <> oldConfigItemName

-- this is almost certainly not the proper way to do this with reflex...
fillDialog :: ProviderDialogInfo t -> ConfigItem -> IO ()
fillDialog dialogInfo ConfigItem{..} = do
    htmlInputElementSetValue (_textInput_element $ pdSourceNameEntry dialogInfo) configItemName
    forM_ (HashMap.keys configuration) $ \key -> do
        let txtInput = fromJust $ Map.lookup (T.unpack key) (pdTextEntries dialogInfo)
        htmlInputElementSetValue (_textInput_element txtInput) (readObjectField (T.unpack key) configuration)

readDialog :: ProviderDialogInfo t -> PluginConfig -> IO ConfigItem
readDialog dialogInfo PluginConfig{..} = do
    newName <- htmlInputElementGetValue $ _textInput_element $ pdSourceNameEntry dialogInfo
    cfgList <- sequence $ flip map cfgPluginConfig $ \cfgDataInfo -> do
        let mName = memberName cfgDataInfo
        let inputField = fromJust $ Map.lookup mName $ pdTextEntries dialogInfo
        val <- htmlInputElementGetValue (_textInput_element inputField)
        return (T.pack mName, A.String $ T.pack val)
    return $ ConfigItem newName cfgPluginName (HashMap.fromList cfgList)

readObjectField :: String -> A.Object -> String
readObjectField fieldName aObject = fromMaybe "" (A.parseMaybe (\obj -> obj .: T.pack fieldName) aObject)

pluginContents :: MonadWidget t m => PluginConfig -> ConfigItem -> m ()
pluginContents pluginConfig configContents = elAttr "table" ("class" =: "table") $
    mapM_ (getPluginElement $ configuration configContents) (cfgPluginConfig pluginConfig)

getPluginElement :: MonadWidget t m => A.Object -> ConfigDataInfo -> m ()
getPluginElement config dataInfo = do
    let memberNameV = memberName dataInfo
    let memberValue = readObjectField memberNameV config
    let memberValueDisplay = case memberType dataInfo of
            "Password" -> replicate (length memberValue) '*'
            _          -> memberValue
    el "tr" $ do
        el "td" $ text memberNameV
        el "td" $ text memberValueDisplay
