{-# LANGUAGE RecordWildCards, RecursiveDo, DeriveGeneric, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections #-}

module Config where

import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Types (fromJSString)

import Reflex.Dom hiding (display)

import Data.Aeson as A
import Data.Aeson.Types as A
import GHC.Generics
import Data.List
import Data.Function
import Data.Maybe
import Data.Bifunctor
import Data.Monoid
import Control.Applicative
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Ord
import Clay as C hiding (map, (&), filter, head, p, url, active,
                         name, pc, id, intersperse, reverse)
import Data.Bool (bool)

import Communication
import EventProvider
import Common
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

configView :: MonadWidget t m => Dynamic t ActiveView -> m ()
configView activeViewDyn = do
    let configStyle = do
            flexGrow 1
            display flex
            flexDirection column
            paddingRight (px 10)
            overflow auto
    attrsDyn <- forDyn activeViewDyn $ \curView ->
        attrStyleWithHideIf (curView /= ActiveViewConfig) configStyle
    elDynAttr "div" attrsDyn $ do
        -- TODO getPostBuild ... maybe when the tab is loaded instead?
        postBuild  <- getPostBuild
        cfgDescDyn <- makeSimpleXhr "/configdesc" postBuild
        cfgValDyn  <- makeSimpleXhr "/configVal" postBuild
        readAllDyn <- combineDyn (liftA2 FetchedData) cfgDescDyn cfgValDyn

        rec
            configDataDyn <- foldDyn ($) RemoteDataLoading $ mergeWith (.)
                [
                    fmap const (updated readAllDyn),
                    fmap (flip applyConfigChange) configUpdateEvt
                ]

            configUpdateEvt <- displayConfig =<< mapDyn fromRemoteData configDataDyn
        return ()

data ConfigUpdate = ConfigUpdate
    {
        oldConfigItemName :: String,
        newConfigItem :: ConfigItem
    } deriving Show

data ConfigChange = ChangeAdd ConfigItem
                  | ChangeUpdate ConfigUpdate
                  | ChangeDelete ConfigItem
                  deriving Show

applyConfigChange :: RemoteData FetchedData -> ConfigChange -> RemoteData FetchedData
applyConfigChange (RemoteData (FetchedData desc val)) chg = RemoteData (FetchedData desc newVal)
    where newVal = case chg of
           ChangeAdd newCi -> newCi:val
           ChangeUpdate (ConfigUpdate oldCiName newCi) ->
               newCi : filter ((/= oldCiName) . configItemName) val
           ChangeDelete ci -> filter (/= ci) val
applyConfigChange _ _ = error "applyConfigChange called on unloaded data??"

displayConfig :: MonadWidget t m => Dynamic t (Maybe FetchedData)
              -> m (Event t ConfigChange)
displayConfig dynFetchedData = do
    rec
        cfgByProvider <- mapDyn (fromMaybe []) =<< mapDyn (liftA groupByProvider) dynFetchedData
        let addCfgBtn = displayAddCfgButton . maybe [] fetchedConfigDesc
        addCfgEvt <- holdDyn never =<< (dyn =<< mapDyn addCfgBtn dynFetchedData)
        cfgChgEvt <- elStyle "div" (flexGrow 1 >> overflow auto) $
            mapDyn leftmost =<< simpleList cfgByProvider displayConfigSection
    return $ leftmost $ fmap (switch . current) [addCfgEvt, cfgChgEvt]

displayAddCfgButton :: MonadWidget t m => [PluginConfig] -> m (Event t ConfigChange)
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
    addEvts <- zipWithM (addCfgPluginAdd Nothing) pluginConfigs clickEvts
    return (leftmost addEvts)

addCfgDropdownBtn :: MonadWidget t m => PluginConfig -> m (Event t ())
addCfgDropdownBtn PluginConfig{..} = do
    (pcLnk, _) <- elAttr' "a" ("class" =: "dropdown-item" <>
                               "href"  =: "javascript:void(0);") $
        text cfgPluginName
    return (domEvent Click pcLnk)

-- TODO obvious duplication in the way the add/edit/delete modals are handled.
-- setupModal then buildModalBody, then error event, save event, then handle save...
addCfgPluginAdd :: MonadWidget t m => Maybe ConfigItem -> PluginConfig -> Event t ()
                -> m (Event t ConfigChange)
addCfgPluginAdd mConfigItem pc clickEvt = do
    let ci = ConfigItem "" "" HashMap.empty
    fieldContentsEvt <- configModalFetchFieldContents clickEvt mConfigItem pc
    fieldContentsDyn <- holdDyn Map.empty fieldContentsEvt
    setupModalR <- setupModal ModalLevelBasic fieldContentsEvt $ do
        rec
            (dialogResult, addDlgOkEvt, _) <- buildModalBody "Add" (PrimaryBtn "Save")
                    errorDyn (editConfigItem pc ci fieldContentsDyn)
            errorDyn <- remoteDataErrorDescDyn saveEvt

            editConfigEvt <- performEvent $ fmap
                (const $ ChangeAdd <$> readDialog dialogResult pc)
                addDlgOkEvt
            saveEvt <- saveConfig editConfigEvt
        return saveEvt
    modalHandleSaveAction ModalLevelBasic setupModalR

groupByProvider :: FetchedData -> [(PluginConfig, [ConfigItem])]
groupByProvider (FetchedData configDesc configVal) =
    fmap (second $ sortBy (comparing configItemName)) $ -- sort config items by name
    fmap (first fromJust) $ filter (isJust . fst) $     -- only keep Just providers.
    fmap (first $ providerByName configDesc) $          -- replace provider name by provider (Maybe)
    buckets providerName configVal                      -- bucket by provider name

providerByName :: [PluginConfig] -> String -> Maybe PluginConfig
providerByName pluginConfigs name = find ((== name) . cfgPluginName) pluginConfigs

type EditConfigItemRender t = (Dynamic t String, Map String (Dynamic t String))

editConfigItem :: MonadWidget t m => PluginConfig -> ConfigItem -> Dynamic t (Map String [String])
               -> m (EditConfigItemRender t)
editConfigItem pc@PluginConfig{..} ConfigItem{..} fieldContentsDyn = do
    when (isJust $ find (== MtPassword) (memberType <$> cfgPluginConfig)) $
        elAttr "div" ("class" =: "alert alert-warning" <> "role" =: "alert") $
            el "strong" (text "Warning") >>
                text " passwords are stored in plain text in the configuration file!"
    el "form" $ do
        rec
            srcNameInput <- elAttr "fieldset" ("class" =: "form-group") $
                fieldEntry "sourceName" "Enter source name:" configItemName
            updatedFieldCts <-
                combineDyn (flip Map.union) fieldContentsDyn updatedDepFieldCtsDyn
            fieldInputs <- mapM (editConfigDataInfo updatedFieldCts configItemName configuration) cfgPluginConfig
            let makeListPair = \(fieldName, valDyn) ->
                     mapDyn (replicate 1 . (fieldName,)) valDyn
            fieldDyns <- fmap nubDyn $
                combineDyns (++) [] =<< mapM makeListPair fieldInputs
            updatedDepFieldCtsDyn <- holdDyn Map.empty =<< dialogChanged pc (updated fieldDyns)
        return (srcNameInput, Map.fromList fieldInputs)

dialogChanged :: MonadWidget t m => PluginConfig -> Event t [(String, String)]
              -> m (Event t (Map String [String]))
dialogChanged pc@PluginConfig{..} evt = do
    let jsonEvt = (encodeToStr . Map.fromList) <$> evt
    let itemsToRefresh = filter ((== DependsOnOthers) . valueType) cfgPluginConfig
    readConfigFieldContents =<< mapM (fetchConfigFieldContents jsonEvt pc) itemsToRefresh

editConfigDataInfo :: MonadWidget t m => Dynamic t (Map String [String]) -> String -> A.Object -> ConfigDataInfo
                   -> m (String, Dynamic t String)
editConfigDataInfo fieldContentsDyn cfgItemName obj ConfigDataInfo{..} = do
    let fieldValue = readObjectField memberName obj
    let displayer = case memberType of
          MtPassword    -> passwordEntry
          MtFolderPath  -> fileEntry PickFolder cfgItemName
          MtFilePath    -> fileEntry PickFile cfgItemName
          MtText        -> fieldEntry
          MtCombo       -> comboEntry fieldContentsDyn
          MtMultiChoice -> multiChoiceEntry fieldContentsDyn
    field <- displayer memberName memberLabel fieldValue
    return (memberName, field)

fileEntry :: MonadWidget t m => PickerOperationMode -> String -> String -> String -> String
          -> m (Dynamic t String)
fileEntry pickerOpMode _ memberName memberLabel val = do
    elAttr "label" ("for" =: memberName) $ text memberLabel
    elAttr "div" ("class" =: "input-group") $ do
        rec
            let inputAttrs = "id" =: memberName <> "class" =: "form-control"
            inputVal <- _textInput_value <$> textInput
                (def
                 & textInputConfig_attributes .~ constDyn inputAttrs
                 & textInputConfig_initialValue .~ val
                 & textInputConfig_setValue .~ updatedFilePath)
            (browseBtn, _) <- elAttr' "div" ("class" =: "input-group-addon") $
                elStyle' "span" (cursor pointer) $ text "Browse..."
            updatedFilePath <- buildFilePicker
                pickerDefaultOptions { pickerMode = pickerOpMode }
                (tagDyn inputVal $ domEvent Click browseBtn)
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
            attrsDyn <- forDyn showPaswd $ \p ->
                "class" =: "form-control" <>
                "id"    =: fieldId <>
                "value" =: fieldValue <>
                "type"  =: if p then "password" else "text"
            (inputField, _) <- elDynAttr' "input" attrsDyn $ return ()
            showPaswd <- toggle True (domEvent Click padlock)
            (padlock, _) <- elAttr' "div" ("class" =: "input-group-addon") $
                rawPointerSpan =<< forDyn showPaswd (bool "&#128275;" "&#128274;")
        let getFieldValue = liftIO $ do
                val <- getValue (castToHTMLInputElement $ _el_element inputField)
                return $ fromMaybe "" $ fromJSString <$> val
        holdDyn fieldValue =<< performEvent
            (const getFieldValue <$> domEvent Change inputField)

-- i have to give a map to reflex, and it sorts -- I want
-- case insensitive sorting so I have no choice but this.
-- probably should rather use int for the key or something...
newtype CaseFoldString = CaseFoldString { unCaseFold :: String } deriving (Show, Read)

cfsToCaseFold :: CaseFoldString -> T.Text
cfsToCaseFold = T.toCaseFold . T.pack . unCaseFold

instance Ord CaseFoldString where
    compare = compare `on` cfsToCaseFold
instance Eq CaseFoldString where
    (==) = (==) `on` cfsToCaseFold

comboEntry :: MonadWidget t m => Dynamic t (Map String [String])
           -> String -> String -> String
           -> m (Dynamic t String)
comboEntry fieldContentsDyn memberName memberLabel fieldValue = do
    let toKeyVal x = (CaseFoldString x,x)
    let prepareComboCts = Map.fromList . map toKeyVal . fromJust . Map.lookup memberName
    itemsDyn <- mapDyn prepareComboCts fieldContentsDyn
    elAttr "label" ("for" =: memberName) $ text memberLabel
    elAttr "div" ("class" =: "input-group") $ do
        val <- _dropdown_value <$> dropdown (CaseFoldString fieldValue) itemsDyn
            (def & dropdownConfig_attributes .~ constDyn ("class" =: "form-control"))
        mapDyn unCaseFold val

multiChoiceEntry :: MonadWidget t m => Dynamic t (Map String [String])
           -> String -> String -> String
           -> m (Dynamic t String)
multiChoiceEntry fieldContentsDyn memberName memberLabel fieldValue = do
    evtDyn <- dyn =<< mapDyn
        (multiChoiceEntry_ memberName memberLabel fieldValue)
        (nubDyn fieldContentsDyn)
    joinDyn <$> holdDyn (constDyn "") evtDyn

multiChoiceEntry_ :: MonadWidget t m => String -> String -> String
           -> Map String [String]
           -> m (Dynamic t String)
multiChoiceEntry_ memberName memberLabel fieldValue fieldContents = do
    let active = T.unpack <$> T.splitOn "|" (T.pack fieldValue)
    el "label" $ text memberLabel
    let valueList = fromJust $ Map.lookup memberName fieldContents
    elStyle "div" (overflow auto >> height (px 150)) $ do
        rec
            currentSelection <- foldDyn
                (\(isOn, val) values ->
                   if isOn
                   then val : values
                   else delete val values) active clickedCbEvt
            clickedCbEvt <- leftmost <$> mapM (singleCb currentSelection) valueList
        mapDyn (intercalate "|") currentSelection

singleCb :: MonadWidget t m => Dynamic t [String] -> String -> m (Event t (Bool, String))
singleCb activeListDyn txt = do
    isActiveDyn <- mapDyn (elem txt) activeListDyn
    cbEvt <- el "label" (checkboxView (constDyn Map.empty) isActiveDyn <* text txt)
    el "br" (return ())
    return $ fmap (, txt) cbEvt

getConfigValue :: MonadWidget t m => Event t String -> PluginConfig -> String
    -> m (Dynamic t (RemoteData [String]))
getConfigValue evt pluginConfig cfgItemName = do
    let url = "/configFetchFieldContents/"
            <> cfgPluginName pluginConfig
            <> "?configItemName=" <> cfgItemName
    let xhrReq dataJson = xhrRequest "POST" url $
                          def { _xhrRequestConfig_sendData = Just dataJson }
    req <- performRequestAsync $ xhrReq <$> evt
    holdDyn RemoteDataLoading $ fmap readRemoteData req

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
    dynParams <- combineDyn (\cfg dataInfos -> map (cfg,) dataInfos) dynPluginConfig dynConfigItems
    elAttr "div" ("class" =: "card") $ do
        elAttr "h5" ("class" =: "card-header") $ dynText =<< mapDyn cfgPluginName dynPluginConfig
        elAttr "div" ("class" =: "card-block") $ do
            dynEvtsAr <- mapDyn (sequence . fmap (uncurry displaySectionItem)) dynParams
            dynEvts <- fmap leftmost <$> dyn dynEvtsAr
            (switch . current) <$> holdDyn never dynEvts

displaySectionItem :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigChange)
displaySectionItem pluginConfig ci@ConfigItem{..} =
    elAttr "div" ("class" =: "card") $ do
        cfgChgEvt <- elAttr "div" ("class" =: "card-header") $ do
            text configItemName
            delEvt <- addDeleteButton ci
            updEvt <- addEditButton pluginConfig ci
            -- leftmost is ok, they can't both happen at the same time
            return $ leftmost [delEvt, updEvt]
        elAttr "div" ("class" =: "card-block") $
            pluginContents pluginConfig ci
        return cfgChgEvt

addDeleteButton :: MonadWidget t m => ConfigItem -> m (Event t ConfigChange)
addDeleteButton ci@ConfigItem{..} = do
    (deleteBtn, _) <- elAttrStyle' "button"
        ("class" =: "btn btn-danger btn-sm") (float floatRight) $ text "Delete"
    setupModalR <- setupModal ModalLevelBasic (domEvent Click deleteBtn) $ do
        rec
            (_, deleteDlgOkEvt, _) <- buildModalBody "Delete" (DangerBtn "Delete")
                errorDyn (text $ "Delete the config item " <> configItemName <> "?")
            errorDyn <- remoteDataErrorDescDyn saveEvt

            let deleteEvt = fmap (const $ ChangeDelete ci) deleteDlgOkEvt
            saveEvt <- saveConfig deleteEvt
        return saveEvt
    modalHandleSaveAction ModalLevelBasic setupModalR

addEditButton :: MonadWidget t m => PluginConfig -> ConfigItem -> m (Event t ConfigChange)
addEditButton pluginConfig ci@ConfigItem{..} = do
    let btnClass = "class" =: "btn btn-default btn-sm"
    let btnStyle = float floatRight >> marginRight (px 5)
    (editBtn, _) <- elAttrStyle' "button" btnClass btnStyle $ text "Edit"
    fieldContentsEvt <- configModalFetchFieldContents (domEvent Click editBtn) (Just ci) pluginConfig
    fieldContentsDyn <- holdDyn Map.empty fieldContentsEvt
    setupModalR <- setupModal ModalLevelBasic fieldContentsEvt $ do
        rec
            (dialogResult, editDlgOkEvt, _) <- buildModalBody "Edit" (PrimaryBtn "Save")
                errorDyn (editConfigItem pluginConfig ci fieldContentsDyn)
            errorDyn <- remoteDataErrorDescDyn saveEvt

            editConfigEvt <- performEvent $ fmap
                (const $ ChangeUpdate . ConfigUpdate configItemName <$>
                 readDialog dialogResult pluginConfig)
                editDlgOkEvt
            saveEvt <- saveConfig editConfigEvt
        return saveEvt
    modalHandleSaveAction ModalLevelBasic setupModalR

configModalFetchFieldContents :: MonadWidget t m => Event t () -> Maybe ConfigItem -> PluginConfig
                              -> m (Event t (Map String [String]))
configModalFetchFieldContents evt mConfigItem pc@PluginConfig{..} = do
    let configJson = case mConfigItem of
          Nothing -> ""
          Just ci -> encodeToStr $ configuration ci
    let needsPrefetch = flip elem [MtCombo, MtMultiChoice] . memberType
    let jsonEvt = const configJson <$> evt
    case filter needsPrefetch cfgPluginConfig of
      []   -> return (const Map.empty <$> evt)
      cdis -> readConfigFieldContents =<<
          mapM (fetchConfigFieldContents jsonEvt pc) cdis

readConfigFieldContents :: MonadWidget t m
                        => [Dynamic t (RemoteData [(String, [String])])]
                        -> m (Event t (Map String [String]))
readConfigFieldContents requests = do
    remoteData <- combineDyns (combineRemoteData (++)) (RemoteData []) requests
    return $ Map.fromList <$> fmapMaybe fromRemoteData (updated remoteData)

fetchConfigFieldContents :: MonadWidget t m
                         => Event t String -> PluginConfig -> ConfigDataInfo
                         -> m (Dynamic t (RemoteData [(String, [String])]))
fetchConfigFieldContents evt pluginConfig ConfigDataInfo{..} =
    mapDyn (fmap (replicate 1 . (memberName,))) =<<
        getConfigValue evt pluginConfig memberName

modalHandleSaveAction :: MonadWidget t m => ModalLevel -> Event t (RemoteData b)
                      -> m (Event t b)
modalHandleSaveAction modalLevel saveEvt = do
    let savedCfgEditEvt = fmapMaybe fromRemoteData saveEvt
    hideModalOnEvent modalLevel savedCfgEditEvt
    return savedCfgEditEvt

encodeToStr :: ToJSON a => a -> String
encodeToStr = bsToStr . encode
    where bsToStr = map (chr . fromEnum) . BS.unpack

saveConfig :: MonadWidget t m => Event t ConfigChange -> m (Event t (RemoteData ConfigChange))
saveConfig configAddEvt = do
    let makeReq = \case
            (ChangeAdd cfg) -> do
                let url = "/config"
                xhrRequest "POST" url $
                    def { _xhrRequestConfig_sendData = Just (encodeToStr cfg) }
            (ChangeUpdate cfgEdit) -> do
                let url = "/config?oldConfigItemName=" <> oldConfigItemName cfgEdit
                xhrRequest "PUT" url $
                    def { _xhrRequestConfig_sendData = Just (encodeToStr $ newConfigItem cfgEdit) }
            (ChangeDelete cfg) -> do
                let url = "/config?configItemName=" <> configItemName cfg
                xhrRequest "DELETE" url def
    httpVoidRequest makeReq configAddEvt

httpVoidRequest :: MonadWidget t m => (a -> XhrRequest) -> Event t a
                -> m (Event t (RemoteData a))
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
readObjectField fieldName aObject = fromMaybe ""
    (A.parseMaybe (\obj -> obj .: T.pack fieldName) aObject)

pluginContents :: MonadWidget t m => PluginConfig -> ConfigItem -> m ()
pluginContents pluginConfig configContents = elAttr "table" ("class" =: "table") $
    mapM_ (getPluginElement $ configuration configContents) (cfgPluginConfig pluginConfig)

getPluginElement :: MonadWidget t m => A.Object -> ConfigDataInfo -> m ()
getPluginElement config ConfigDataInfo{..} = do
    let memberValue = readObjectField memberName config
    let memberValueDisplay = case memberType of
            MtPassword -> replicate (length memberValue) '*'
            _          -> memberValue
    el "tr" $ do
        el "td" $ text memberLabel
        el "td" $ text memberValueDisplay
