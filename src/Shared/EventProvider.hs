{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
module EventProvider (
    GlobalSettings(GlobalSettings), EventProvider(..), MemberType(..),
    eventProviderWrap, getSettingsFolder, deriveConfigRecord,
    ConfigDataType(..), ConfigDataInfo(..), ContentType, FolderPath, Url) where

import Data.Time.Calendar
import Data.Aeson
import Language.Haskell.TH
import Data.ByteString (ByteString)
import Control.Error
import GHC.Generics
import Control.Monad
import Data.Text (Text)

import TsEvent

data MemberType = MtFilePath | MtFolderPath | MtPassword | MtText | MtCombo
    deriving (Eq, Show, Generic)
instance ToJSON MemberType
instance FromJSON MemberType

data ConfigDataInfo = ConfigDataInfo
    {
        memberName  :: String,
        memberLabel :: String,
        memberType  :: MemberType
    } deriving (Eq, Show, Generic)
instance ToJSON ConfigDataInfo
instance FromJSON ConfigDataInfo

deriveConfigRecord :: ConfigDataType -> Q [Dec]
deriveConfigRecord (ConfigDataType providerName cfgMembers) = do
    let cfgDataName = mkName (providerName ++ "ConfigRecord")
    let ctrName  = mkName (providerName ++ "ConfigRecord")
    fields <- forM cfgMembers createConfigRecordField
    return [DataD [] cfgDataName [] [RecC ctrName fields] []]

createConfigRecordField :: ConfigDataInfo -> Q (Name, Strict, Type)
createConfigRecordField (ConfigDataInfo name _ mType) = do
    let fieldName = mkName name
    Just typeName <- lookupTypeName $ case mType of
      MtText       -> "Text"
      MtPassword   -> "Text"
      MtFilePath   -> "String"
      MtFolderPath -> "String"
      MtCombo      -> "Text"
    return (fieldName, NotStrict, ConT typeName)

data ConfigDataType = ConfigDataType
    {
        dataName :: String,
        members  :: [ConfigDataInfo]
    } deriving (Eq, Show, Generic)
instance ToJSON ConfigDataType

data GlobalSettings = GlobalSettings { getSettingsFolder :: String }

type FolderPath  = String
type ContentType = String
type Url         = String

data EventProvider a b = EventProvider {
    getModuleName :: String,
    getEvents     :: a -> GlobalSettings -> Day -> (b -> Url) -> ExceptT String IO [TsEvent],
    getConfigType :: [ConfigDataInfo],
    getExtraData  :: Maybe (a -> GlobalSettings -> b -> IO (Maybe (ContentType, ByteString))),
    fetchFieldCts :: Maybe (ConfigDataInfo -> Maybe a -> GlobalSettings -> IO [Text])
}

instance Show (EventProvider a b) where show = getModuleName

decodeVal :: FromJSON a => Value -> a
decodeVal value = case fromJSON value of
    Error msg -> error msg
    Success a -> a

-- workaround for heteregenous lists. I hate this.
eventProviderWrap :: (FromJSON a, ToJSON a, FromJSON b, ToJSON b) =>  EventProvider a b
                  -> EventProvider Value Value
eventProviderWrap (EventProvider innerGetModName innerGetEvents
        innerGetConfigType innerGetExtraData innerFetchFieldCts) = EventProvider
    {
        getModuleName = innerGetModName,
        getEvents = \a s d u -> innerGetEvents (decodeVal a) s d (u . toJSON),
        getConfigType = innerGetConfigType,
        getExtraData = innerGetExtraData >>= \decoder ->
            Just $ \cfg s k -> decoder (decodeVal cfg) s (decodeVal k),
        fetchFieldCts = innerFetchFieldCts >>= \fetcher ->
            Just $ \cdi cfg s -> fetcher cdi (decodeVal <$> cfg) s
    }
