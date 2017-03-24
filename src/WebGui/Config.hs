{-# LANGUAGE RecordWildCards, RecursiveDo, DeriveGeneric, LambdaCase, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, TypeFamilies, TemplateHaskell #-}

module Config where

import Reflex.Dom hiding (display, Value)

import Data.Aeson as A
import GHC.Generics
import Data.List
import Data.Function
import Data.Maybe
import Data.Bifunctor
import Data.Monoid
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Monad
import Control.Lens
import Data.Ord
import Clay as C hiding (map, (&), a, filter, head, p, url, active,
                         name, pc, id, intersperse, reverse, Value)
import Data.String.Conversions
import Control.Monad.Trans

import Communication
import EventProvider
import Common
import ConfigWidgets
import FilePicker

-- TODO stop copy-pasting this between client & server
data ConfigItem = ConfigItem
    {
        configItemName :: String,
        providerName   :: String,
        configuration  :: A.Object
    } deriving (Eq, Show, Generic)
instance FromJSON ConfigItem
instance ToJSON ConfigItem

data FetchedData = FetchedData
    {
        fetchedConfigDesc :: [PluginConfig],
        fetchedConfigVal  :: [ConfigItem]
    } deriving Show

data ConfigUpdate = ConfigUpdate
    {
        oldConfigItemName :: String,
        newConfigItem     :: ConfigItem
    } deriving Show

data ConfigChangeRequest = ChangeAddRequest PluginConfig
                  | ChangeUpdateRequest PluginConfig ConfigItem
                  | ChangeDeleteRequest ConfigItem
                  deriving Show
$(makePrisms ''ConfigChangeRequest)

data ConfigChange = ChangeAdd ConfigItem
                  | ChangeUpdate ConfigUpdate
                  | ChangeDelete ConfigItem
                  deriving Show

configView :: MonadWidget t m => Dynamic t ActiveView -> m ()
configView activeViewDyn = do
    let configStyle = do
            flexGrow 1
            display flex
            flexDirection column
            paddingRight (px 10)
            overflow auto
    let attrsDyn = ffor activeViewDyn $ \curView ->
          attrStyleWithHideIf (curView /= ActiveViewConfig) configStyle
    elDynAttr "div" attrsDyn $ do
        -- TODO getPostBuild ... maybe when the tab is loaded instead?
        postBuild  <- getPostBuild
        cfgDescDyn <- makeSimpleXhr "/configdesc" postBuild
        cfgValDyn  <- makeSimpleXhr "/configVal" postBuild
        let readAllDyn = zipDynWith (liftA2 FetchedData) cfgDescDyn cfgValDyn

        rec
            configDataDyn <- foldDyn ($) RemoteDataLoading $ mergeWith (.)
                [
                    fmap const (updated readAllDyn),
                    fmap (flip applyConfigChange) configUpdateEvt,
                    fmap (flip applyConfigChange) configAddEvt,
                    fmap (flip applyConfigChange) configDelEvt
                ]
            configUpdateEvt <- displayEditPopup (fmapMaybe (preview _ChangeUpdateRequest) configUpdateReqEvt)
            configAddEvt    <- displayAddPopup  (fmapMaybe (preview _ChangeAddRequest) configUpdateReqEvt)
            configDelEvt    <- displayDeletePopup (fmapMaybe (preview _ChangeDeleteRequest) configUpdateReqEvt)

            configUpdateReqEvt <- displayConfig (fmap fromRemoteData configDataDyn)
        return ()

applyConfigChange :: RemoteData FetchedData -> ConfigChange -> RemoteData FetchedData
applyConfigChange (RemoteData (FetchedData desc val)) chg = RemoteData (FetchedData desc newVal)
    where newVal = case chg of
           ChangeAdd newCi -> newCi:val
           ChangeUpdate (ConfigUpdate oldCiName newCi) ->
               newCi : filter ((/= oldCiName) . configItemName) val
           ChangeDelete ci -> filter (/= ci) val
applyConfigChange _ _ = error "applyConfigChange called on unloaded data??"

displayConfig :: MonadWidget t m => Dynamic t (Maybe FetchedData) -> m (Event t ConfigChangeRequest)
displayConfig dynFetchedData = do
    rec
        let cfgByProvider = fromMaybe [] . liftA groupByProvider <$> dynFetchedData
        let addCfgBtn = displayAddCfgButton . maybe [] fetchedConfigDesc
        addCfgEvt <- holdDyn never =<< dyn (addCfgBtn <$> dynFetchedData)
        cfgChgEvt <- elStyle "div" (flexGrow 1 >> overflow auto) $
                    fmap leftmost <$> simpleList cfgByProvider displayConfigSection
    return $ leftmost $ fmap (switch . current) [addCfgEvt, cfgChgEvt]

displayAddCfgButton :: MonadWidget t m => [PluginConfig] -> m (Event t ConfigChangeRequest)
displayAddCfgButton pluginConfigs = do
    let addBtnStyle = do
            display flex
            flexDirection row
            justifyContent flexEnd
            flexShrink 0
            paddingRight (px 30)
            paddingBottom (px 10)
    clickEvts <- elAttrStyle "div" ("width" =: "100%") addBtnStyle $
        elAttr "div" ("class" =: "btn-group") $ do
            elAttr "button" ("type"  =: "button" <>
                             "class" =: "btn btn-primary dropdown-toggle" <>
                             "data-toggle"   =: "dropdown" <>
                             "aria-haspopup" =: "true" <>
                             "aria-expanded" =: "false")
                $ text "Add..."
            elAttr "div" ("class" =: "dropdown-menu dropdown-menu-right") $
                mapM addCfgDropdownBtn pluginConfigs
    -- addEvts <- zipWithM (addCfgPluginAdd divElt Nothing) pluginConfigs clickEvts
    let addEvts = zipWith (\cfg evt -> (const $ ChangeAddRequest cfg) <$> evt) pluginConfigs clickEvts
    return (leftmost addEvts)

addCfgDropdownBtn :: MonadWidget t m => PluginConfig -> m (Event t ())
addCfgDropdownBtn PluginConfig{..} = do
    (pcLnk, _) <- elAttr' "a" ("class" =: "dropdown-item" <>
                               "href"  =: "javascript:void(0);") $
        text (T.pack cfgPluginName)
    return (domEvent Click pcLnk)

-- TODO pretty huge duplication between displayAddPopup and displayEditPopup
displayAddPopup :: MonadWidget t m => Event t PluginConfig -> m (Event t ConfigChange)
displayAddPopup addReqEvt = do
    let addReqCiEvt = (, Nothing) <$> addReqEvt
    fieldContentsEvt <- configModalFetchFieldContents addReqCiEvt
    fieldContentsDyn <- holdDyn Map.empty fieldContentsEvt
    rec
        let ci = ConfigItem "" "" HashMap.empty
        dynPcCi <- holdDyn Nothing $ Just . (,ci) <$> addReqEvt
        let contentsDyn = join $ ffor dynPcCi $ \pcCi ->
              return (editConfigItem pcCi fieldContentsDyn)
        (dlgResult, dlgClose) <-
            buildModalBody fieldContentsEvt "Add" (PrimaryBtn "Save") errorDyn contentsDyn
        let dialogResultDyn = dlgContentsDyn dlgResult
        let addDlgOkEvt = dlgOkEvt dlgResult
        errorDyn <- remoteDataErrorDescDyn saveEvt

        let dataDyn = zipDynWith (\a mb -> (a,) <$> mb) dialogResultDyn dynPcCi

        addConfigEvt <- performEvent $ fmap
            (\(dialogResult, (pluginConfig, _)) -> ChangeAdd <$>
             readDialog dialogResult pluginConfig)
            $ fmapMaybe id
            $ tagPromptlyDyn dataDyn addDlgOkEvt
        saveEvt <- saveConfig addConfigEvt
    -- TODO this bit of code is duplicated in a couple of spots
    let cfgChange = fmapMaybe fromRemoteData saveEvt
    performEvent_ $ const (liftIO dlgClose) <$> cfgChange
    return cfgChange

groupByProvider :: FetchedData -> [(PluginConfig, [ConfigItem])]
groupByProvider (FetchedData configDesc configVal) =
    fmap (second $ sortBy (comparing configItemName)) $ -- sort config items by name
    fmap (first fromJust) $ filter (isJust . fst) $     -- only keep Just providers.
    fmap (first $ providerByName configDesc) $          -- replace provider name by provider (Maybe)
    buckets providerName configVal                      -- bucket by provider name

providerByName :: [PluginConfig] -> String -> Maybe PluginConfig
providerByName pluginConfigs name = find ((== name) . cfgPluginName) pluginConfigs

type EditConfigItemRender t = (Dynamic t Text, Map String (Dynamic t Value))

editConfigItem :: MonadWidget t m => Maybe (PluginConfig, ConfigItem) -> Dynamic t (Map Text [Text])
               -> m (EditConfigItemRender t)
editConfigItem Nothing _ = return (constDyn "", Map.empty)
editConfigItem (Just (pc, ci)) fieldContentsDyn = do
    when (isJust $ find (== MtPassword) (memberType <$> cfgPluginConfig pc)) $
        elAttr "div" ("class" =: "alert alert-warning" <> "role" =: "alert") $
            el "strong" (text "Warning") >>
                text " passwords are stored in plain text in the configuration file!"
    el "form" $ do
        rec
            srcNameInput <- elAttr "fieldset" ("class" =: "form-group") $
                fieldEntry "sourceName" "Source name" (T.pack $ configItemName ci)
            let updatedFieldCts = zipDynWith (flip Map.union) fieldContentsDyn updatedDepFieldCtsDyn
            fieldInputs <- mapM (editConfigDataInfo updatedFieldCts (configItemName ci) (configuration ci)) $ cfgPluginConfig pc
            let makeListPair = \(fieldName, valDyn) -> fmap (replicate 1 . (fieldName,)) valDyn
            let fieldDyns = uniqDyn $ combineDyns (++) [] $ makeListPair <$> fieldInputs
            updatedDepFieldCtsDyn <- holdDyn Map.empty =<< dialogChanged pc (updated fieldDyns)
        return (srcNameInput, Map.fromList fieldInputs)

dialogChanged :: MonadWidget t m => PluginConfig -> Event t [(String, Value)]
              -> m (Event t (Map Text [Text]))
dialogChanged pc@PluginConfig{..} evt = do
    let jsonEvt = (encodeToStr . Map.fromList) <$> evt
    let itemsToRefresh = filter ((== DependsOnOthers) . valueType) cfgPluginConfig
    readConfigFieldContents <$> mapM (fetchConfigFieldContents jsonEvt pc) itemsToRefresh

editConfigDataInfo :: MonadWidget t m => Dynamic t (Map Text [Text]) -> String -> A.Object -> ConfigDataInfo
                   -> m (String, Dynamic t Value)
editConfigDataInfo fieldContentsDyn cfgItemName obj ConfigDataInfo{..} = do
    let fieldValue = HashMap.lookup (T.pack memberName) obj
    -- TODO would be better if the individual widgets returned
    -- their precise type and not Value, and I would have
    -- a type index widget type->return type...
    let valToString f name lbl val =
            fmap A.String <$> f name lbl (fromMaybe "" $ valueToStr =<< val)
    let displayer = case memberType of
          MtPassword    -> valToString passwordEntry
          MtFolderPath  -> valToString (fileEntry PickFolder cfgItemName)
          MtFilePath    -> valToString (fileEntry PickFile cfgItemName)
          MtText        -> valToString fieldEntry
          MtCombo       -> valToString (comboEntry fieldContentsDyn)
          MtMultiChoice -> multiChoiceEntry fieldContentsDyn
    field <- elAttr "fieldset" ("class" =: "form-group") $
             displayer (T.pack memberName) (T.pack memberLabel) fieldValue
    return (memberName, field)

getConfigReq :: String -> PluginConfig -> ConfigDataInfo -> XhrRequest Text
getConfigReq dataJson pluginConfig ConfigDataInfo{..} = do
    let url = "/configFetchFieldContents/"
            <> cfgPluginName pluginConfig
            <> "?configItemName=" <> memberName
    xhrRequest "POST" (T.pack url) $
        def { _xhrRequestConfig_sendData = T.pack dataJson }

getConfigValue :: MonadWidget t m => Event t String -> PluginConfig -> ConfigDataInfo
               -> m (Dynamic t (RemoteData [Text]))
getConfigValue dataJsonEvt pc ci = do
    let reqEvt = ffor dataJsonEvt $ \dataJson -> getConfigReq dataJson pc ci
    req <- performRequestAsync reqEvt
    holdDyn RemoteDataLoading $ fmap readRemoteData req

buckets :: Ord b => (a -> b) -> [a] -> [(b, [a])]
buckets f = map (\g -> (fst $ head g, map snd g))
          . groupBy ((==) `on` fst)
          . sortBy (comparing fst)
          . map (\x -> (f x, x))

displayConfigSection :: MonadWidget t m => Dynamic t (PluginConfig, [ConfigItem])
                     -> m (Event t ConfigChangeRequest)
displayConfigSection dynSecInfo_ = do
    let dynSecInfo = uniqDyn dynSecInfo_
    let dynPluginConfig = fst <$> dynSecInfo
    let dynConfigItems  = snd <$> dynSecInfo
    let dynParams = zipDynWith (\cfg dataInfos -> map (cfg,) dataInfos) dynPluginConfig dynConfigItems
    elAttr "div" ("class" =: "card") $ do
        elAttr "h5" ("class" =: "card-header") $ dynText (T.pack . cfgPluginName <$> dynPluginConfig)
        elAttr "div" ("class" =: "card-block") $ do
            let dynEvtsAr = fmap (sequence . fmap (uncurry displaySectionItem)) dynParams
            dynEvts <- fmap leftmost <$> dyn dynEvtsAr
            (switch . current) <$> holdDyn never dynEvts

displaySectionItem :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigChangeRequest)
displaySectionItem pluginConfig ci@ConfigItem{..} =
    elAttr "div" ("class" =: "card") $ do
        let divStyle = display flex >> flexDirection rowReverse
        cfgChgEvt <- elAttrStyle "div" ("class" =: "card-header") divStyle $ do
            delEvt <- addDeleteButton ci
            updEvt <- addEditButton pluginConfig ci
            elStyle "span" (flexGrow 1) $ text $ T.pack configItemName
            -- leftmost is ok, they can't both happen at the same time
            return $ leftmost [delEvt, updEvt]
        elAttr "div" ("class" =: "card-block") $
            pluginContents pluginConfig ci
        return cfgChgEvt

addDeleteButton :: MonadWidget t m => ConfigItem -> m (Event t ConfigChangeRequest)
addDeleteButton ci@ConfigItem{..} = do
    (deleteBtn, _) <- elAttrStyle' "button"
        ("class" =: "btn btn-danger btn-sm") (marginRight (px 5)) $ text "Delete"
    return $ (const $ ChangeDeleteRequest ci) <$> (domEvent Click deleteBtn)

displayDeletePopup :: MonadWidget t m => Event t ConfigItem -> m (Event t ConfigChange)
displayDeletePopup deleteReqEvt = do
    dynCi <- holdDyn Nothing $ Just <$> deleteReqEvt
    rec
        let contentsDyn = ffor dynCi $ \case
                Nothing -> text ""
                Just ci -> text $ "Delete the config item " <> T.pack (configItemName ci) <> "?"
        (dlgInfo, dlgClose) <- buildModalBody deleteReqEvt "Delete" (DangerBtn "Delete") errorDyn contentsDyn
        let deleteDlgOkEvt = dlgOkEvt dlgInfo
        errorDyn <- remoteDataErrorDescDyn saveEvt

        let deleteEvt = ChangeDelete <$> fmapMaybe id (tagPromptlyDyn dynCi deleteDlgOkEvt)
        saveEvt <- saveConfig deleteEvt
    let cfgChange = fmapMaybe fromRemoteData saveEvt
    performEvent_ $ const (liftIO dlgClose) <$> cfgChange
    return cfgChange

addEditButton :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigChangeRequest)
addEditButton pluginConfig ci@ConfigItem{..} = do
    let btnClass = "class" =: "btn btn-default btn-sm"
    (editBtn, _) <- elAttrStyle' "button" btnClass (marginRight (px 5)) $ text "Edit"
    return $ const (ChangeUpdateRequest pluginConfig ci) <$> domEvent Click editBtn

displayEditPopup :: MonadWidget t m => Event t (PluginConfig, ConfigItem)
    -> m (Event t ConfigChange)
displayEditPopup changeReqEvt = do
    dynPcCi <- holdDyn Nothing $ Just <$> changeReqEvt
    fieldContentsEvt <- configModalFetchFieldContents $ fmap Just <$> changeReqEvt
    fieldContentsDyn <- holdDyn Map.empty fieldContentsEvt
    rec
        let contentsDyn = join $ ffor dynPcCi $ \pcCi ->
              return (editConfigItem pcCi fieldContentsDyn)
        (dlgInfo, dlgClose) <- buildModalBody fieldContentsEvt "Edit" (PrimaryBtn "Save") errorDyn contentsDyn
        let dialogResultDyn = dlgContentsDyn dlgInfo
        let editDlgOkEvt = dlgOkEvt dlgInfo
        errorDyn <- remoteDataErrorDescDyn saveEvt

        let dataDyn = zipDynWith (\a mb -> (a,) <$> mb) dialogResultDyn dynPcCi

        editConfigEvt <- performEvent $ fmap
            (\(dialogResult, (pluginConfig, ci)) -> ChangeUpdate . ConfigUpdate (configItemName ci) <$>
             readDialog dialogResult pluginConfig)
            $ fmapMaybe id
            $ tagPromptlyDyn dataDyn editDlgOkEvt
        saveEvt <- saveConfig editConfigEvt
    let cfgChange = fmapMaybe fromRemoteData saveEvt
    performEvent_ $ const (liftIO dlgClose) <$> cfgChange
    return cfgChange

configModalFetchFieldContents :: MonadWidget t m => Event t (PluginConfig, Maybe ConfigItem)
                              -> m (Event t (Map Text [Text]))
configModalFetchFieldContents changeReqEvt = do
    let configJson = \case
          Nothing -> ""
          Just ci -> encodeToStr $ configuration ci
    let needsPrefetch  = flip elem [MtCombo, MtMultiChoice] . memberType
    let prefetchCis pc =  filter needsPrefetch (cfgPluginConfig pc)
    let prefetchReqs pc mCfgItem = getConfigReq (configJson mCfgItem) pc <$> prefetchCis pc
    fieldXhrRespEvt <- performRequestsAsync $ uncurry prefetchReqs <$> changeReqEvt
    fieldXhrDataDyn <- holdDyn [] $ map readRemoteData <$> fieldXhrRespEvt
    configItemsDyn  <- holdDyn [] $ prefetchCis . fst <$> changeReqEvt
    let fetchedDyn = zipDynWith (zipWith (\ci rtTexts -> fmap (T.pack $ memberName ci,) rtTexts)) configItemsDyn fieldXhrDataDyn
    return $ Map.fromList <$> fmapMaybe fromRemoteData (sequence <$> updated fetchedDyn)

readConfigFieldContents :: Reflex t
                        => [Dynamic t (RemoteData [(Text, [Text])])]
                        -> Event t (Map Text [Text])
readConfigFieldContents requests = Map.fromList <$> fmapMaybe fromRemoteData (updated remoteData)
  where remoteData = combineDyns (combineRemoteData (++)) (RemoteData []) requests

fetchConfigFieldContents :: MonadWidget t m
                         => Event t String -> PluginConfig -> ConfigDataInfo
                         -> m (Dynamic t (RemoteData [(Text, [Text])]))
fetchConfigFieldContents evt pluginConfig cfgDataInfo =
    fmap (fmap (replicate 1 . (T.pack (memberName cfgDataInfo),))) <$>
        getConfigValue evt pluginConfig cfgDataInfo

encodeToStr :: ToJSON a => a -> String
encodeToStr = convertString . encode

saveConfig :: MonadWidget t m => Event t ConfigChange -> m (Event t (RemoteData ConfigChange))
saveConfig configAddEvt = do
    let makeReq = \case
            (ChangeAdd cfg) -> do
                let url = "/config"
                xhrRequest "POST" url $
                    def { _xhrRequestConfig_sendData = encodeToStr cfg }
            (ChangeUpdate cfgEdit) -> do
                let url = "/config?oldConfigItemName=" <> T.pack (oldConfigItemName cfgEdit)
                xhrRequest "PUT" url $
                    def { _xhrRequestConfig_sendData = encodeToStr (newConfigItem cfgEdit) }
            (ChangeDelete cfg) -> do
                let url = "/config?configItemName=" <> T.pack (configItemName cfg)
                xhrRequest "DELETE" url $
                    def { _xhrRequestConfig_sendData = "" }
    httpVoidRequest makeReq configAddEvt

httpVoidRequest :: (MonadWidget t m, IsXhrPayload p) => (a -> XhrRequest p) -> Event t a
                -> m (Event t (RemoteData a))
httpVoidRequest makeReq evt = do
    -- take advantage of the Traversable instance for pairs, which
    -- will apply the function to the second element only, to pass
    -- on the ConfigUpdate besides the XhrResponse in the resulting event.
    let reqEvt = fmap (\x -> (x, makeReq x)) evt
    resp <- performRequestsAsync reqEvt
    return $ fmap (\(cu, rsp) -> const cu <$> readEmptyRemoteData rsp) resp

readDialog :: (MonadSample t m, Reflex t) =>
              EditConfigItemRender t -> PluginConfig
              -> m ConfigItem
readDialog (nameInput, cfgInputs) PluginConfig{..} = do
    newName <- sample $ current nameInput
    cfgList <- sequence $ flip map cfgPluginConfig $ \cfgDataInfo -> do
        let mName = memberName cfgDataInfo
        let inputField = fromJust $ Map.lookup mName cfgInputs
        val <- sample $ current inputField
        return (T.pack mName, val)
    return $ ConfigItem (T.unpack newName) cfgPluginName (HashMap.fromList cfgList)

pluginContents :: MonadWidget t m => PluginConfig -> ConfigItem -> m ()
pluginContents pluginConfig configContents = elAttr "table" ("class" =: "table") $
    mapM_ (getPluginElement $ configuration configContents) (cfgPluginConfig pluginConfig)

myValuePrettyPrint :: Value -> String
myValuePrettyPrint (A.String str) = T.unpack str
myValuePrettyPrint (A.Array l) = intercalate ", " $ V.toList $ myValuePrettyPrint <$> l
myValuePrettyPrint x@_ = show x

getPluginElement :: MonadWidget t m => A.Object -> ConfigDataInfo -> m ()
getPluginElement config ConfigDataInfo{..} = do
    let memberValue = fromMaybe "No value" (HashMap.lookup (T.pack memberName) config)
    let memberStr = myValuePrettyPrint memberValue
    let memberValueDisplay = case memberType of
            MtPassword -> replicate (length memberStr) '*'
            _          -> memberStr
    el "tr" $ do
        el "td" $ text $ T.pack memberLabel
        el "td" $ text $ T.pack memberValueDisplay
