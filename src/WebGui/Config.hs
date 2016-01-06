{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, LambdaCase, OverloadedStrings, RecordWildCards, RecursiveDo #-}

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

        rec
            configDataDyn <- foldDyn ($) RemoteDataLoading $ mergeWith (.)
                [
                    fmap const (updated readAllDyn),
                    fmap updateConfigItem configUpdateEvt
                ]

            -- leaving the types here because I find that part confusing.
            -- I think there must be a better way. Asked on #reflex-frp IRC
            -- on 2015-01-02 without answer.
            (render :: Dynamic t (m (Event t ConfigUpdate))) <- mapDyn displayConfig configDataDyn
            (render2 :: Event t (Event t ConfigUpdate)) <- dyn render
            (render3 :: Behavior t (Event t ConfigUpdate)) <- current <$> holdDyn never render2
            let configUpdateEvt = switch render3
        return ()

data ConfigUpdate = ConfigUpdate
     {
         oldConfigItemName :: String,
         newConfigItem :: ConfigItem
     }

updateConfigItem :: ConfigUpdate -> RemoteData FetchedData -> RemoteData FetchedData
updateConfigItem (ConfigUpdate oldCiName newCi) (RemoteData (FetchedData desc val)) =
    RemoteData (FetchedData desc updatedVal)
    where updatedVal = newCi : filter ((/= oldCiName) . configItemName) val
updateConfigItem _ soFar = soFar

displayConfig :: MonadWidget t m => RemoteData FetchedData -> m (Event t ConfigUpdate)
displayConfig RemoteDataLoading = return never
displayConfig (RemoteDataInvalid msg) = text msg >> return never
displayConfig (RemoteData (FetchedData configDesc configVal)) = do
    rec
        let cfgByProvider =
                fmap (first fromJust) $ filter (isJust . fst) $  -- only keep Just providers.
                fmap (first $ providerByName configDesc) $       -- replace provider name by provider (Maybe)
                buckets providerName configVal                   -- bucket by provider name
        editConfigEvt <- leftmost <$> mapM (uncurry displayConfigSection) cfgByProvider
    return editConfigEvt

providerByName :: [PluginConfig] -> String -> Maybe PluginConfig
providerByName pluginConfigs name = find ((== name) . cfgPluginName) pluginConfigs

type EditConfigItemRender t = (TextInput t, Map String (TextInput t))

editConfigItem :: MonadWidget t m => PluginConfig -> ConfigItem -> m (EditConfigItemRender t)
editConfigItem PluginConfig{..} ConfigItem{..} =
    el "form" $ do
        srcNameInput <- elAttr "fieldset" ("class" =: "form-group") $ fieldEntry "sourceName" "Enter source name:" configItemName
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
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))

displayConfigSection :: MonadWidget t m => PluginConfig -> [ConfigItem]
                     -> m (Event t ConfigUpdate)
displayConfigSection secInfo secItems =
    elAttr "div" ("class" =: "card") $ do
        elAttr "h5" ("class" =: "card-header") $ text $ cfgPluginName secInfo
        elAttr "div" ("class" =: "card-block") $
            leftmost <$> mapM (displaySectionItem secInfo) secItems

displaySectionItem :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigUpdate)
displaySectionItem pluginConfig@PluginConfig{..} ci@ConfigItem{..} =
    elAttr "div" ("class" =: "card") $ do
        cfgUpdEvt <- elAttr "div" ("class" =: "card-header") $ do
            text configItemName
            (deleteBtn, _) <- elAttr' "button" ("class" =: "btn btn-danger btn-sm"
                                              <> "style" =: "float: right"
                                              <> "data-toggle" =: "modal"
                                              <> "data-target" =: ("#delete-" <> cfgPluginName)) $ text "Delete"
            addEditButton pluginConfig ci
        elAttr "div" ("class" =: "card-block") $
            pluginContents pluginConfig ci
        return cfgUpdEvt

addEditButton :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigUpdate)
addEditButton pluginConfig@PluginConfig{..} ci@ConfigItem{..} = do
    rec
        dialogInfo <- buildModalDialog cfgPluginName "Edit" "Save"
            (Just errorEvt) (editConfigItem pluginConfig ci)

        (editBtn, _) <- elAttr' "button" ("class" =: "btn btn-default btn-sm"
                                          <> "style" =: "float: right; margin-right: 5px") $ text "Edit"
        performEvent_ $ fmap (const $ liftIO $ showModalDialog dialogInfo) $ domEvent Click editBtn

        let errorEvt = leftmost
                [
                    fmapMaybe remoteDataInvalidDesc saveEvt,
                    -- whenever the user opens the modal, clear the error display.
                    fmap (const "") $ domEvent Click editBtn
                ]

        editConfigEvt <- performEvent $ fmap
            (const $ readDialog (bodyResult dialogInfo) pluginConfig configItemName)
            $ okBtnEvent dialogInfo
        saveEvt <- saveConfigItem editConfigEvt

        let savedCfgChangeEvt = fmapMaybe fromRemoteData saveEvt
        -- request to close the dialog upon success
        performEvent_ $ fmap (const $ liftIO $ hideModalDialog dialogInfo) savedCfgChangeEvt
        -- trigger the config update event to refresh the display
        -- when the dialog is actually closed, and if the save info is present.
        -- (when we refresh, the modal gets removed from the DOM
        -- and doesn't disappear properly, so refresh only after confirmed modal close)
        saveInfoDyn <- holdDyn Nothing $ fmap Just savedCfgChangeEvt
        let cfgUpdEvt = fmapMaybe id $ tagDyn saveInfoDyn (closedEvent dialogInfo)

    return cfgUpdEvt

byteStringToString :: BS.ByteString -> String
byteStringToString = map (chr . fromEnum) . BS.unpack

saveConfigItem :: MonadWidget t m => Event t ConfigUpdate -> m (Event t (RemoteData ConfigUpdate))
saveConfigItem configEditEvent = do
    -- take advantage of the Traversable instance for pairs, which
    -- will apply the function to the second element only, to pass
    -- on the ConfigUpdate besides the XhrResponse in the resulting event.
    let reqEvt = fmap (\x -> (x, buildXhrRequest x)) configEditEvent
    resp <- performRequestsAsync reqEvt
    return $ fmap (\(cu, rsp) -> const cu <$> readEmptyRemoteData rsp) resp

buildXhrRequest :: ConfigUpdate -> XhrRequest
buildXhrRequest ConfigUpdate{..} =
    xhrRequest "PUT" url $ def { _xhrRequestConfig_sendData = xhrData }
    where
      xhrData = Just (byteStringToString $ encode newConfigItem)
      url = "/config?oldConfigItemName=" <> oldConfigItemName

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
getPluginElement config dataInfo = do
    let memberNameV = memberName dataInfo
    let memberValue = readObjectField memberNameV config
    let memberValueDisplay = case memberType dataInfo of
            "Password" -> replicate (length memberValue) '*'
            _          -> memberValue
    el "tr" $ do
        el "td" $ text memberNameV
        el "td" $ text memberValueDisplay
