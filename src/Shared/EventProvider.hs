{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, DeriveGeneric #-}
module EventProvider (thGetTypeDesc,
    GlobalSettings(GlobalSettings), EventProvider(..), MemberType(..),
    eventProviderWrap, getSettingsFolder,
    ConfigDataType(..), ConfigDataInfo(..), FolderPath, ContentType, Url) where

import Data.Time.Calendar
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Data.ByteString (ByteString)
import Control.Error
import GHC.Generics

import TsEvent

data MemberType = MtFilePath | MtFolderPath | MtPassword | MtText
    deriving (Eq, Show, Generic)
instance ToJSON MemberType
instance FromJSON MemberType
$(deriveLift ''MemberType)
instance Read MemberType where
    readsPrec _ "FilePath"   = [(MtFilePath, "")]
    readsPrec _ "FolderPath" = [(MtFolderPath, "")]
    readsPrec _ "Password"   = [(MtPassword, "")]
    readsPrec _ "Text"       = [(MtText, "")]
    readsPrec _ "String"     = [(MtText, "")]
    readsPrec _ _            = []

data ConfigDataInfo = ConfigDataInfo
    {
        memberName :: String,
        memberType :: MemberType
    } deriving (Eq, Show, Generic)
$(deriveLift ''ConfigDataInfo)
instance ToJSON ConfigDataInfo
instance FromJSON ConfigDataInfo

data ConfigDataType = ConfigDataType
    {
        dataName :: String,
        members :: [ConfigDataInfo]
    } deriving (Eq, Show, Generic)
instance ToJSON ConfigDataType

formatTypeName :: Type -> String
formatTypeName (ConT x) = nameBase x
formatTypeName x@_ = error "Don't know how to handle type " ++ show x

showField :: (Name,Type) -> ConfigDataInfo
showField nameType = ConfigDataInfo s (read typS)
    where
        s = nameBase $ fst nameType
        typS = formatTypeName $ snd nameType

showFields :: Name -> [(Name, Type)] -> Q Exp
showFields name names = do
    let exps = fmap showField names
    let nameExp = nameBase name
    [| ConfigDataType nameExp exps |]

thGetTypeDesc :: Name -> Q Exp
thGetTypeDesc name = do
    TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
    let names = map (\(fname,_,typ) -> (fname,typ)) fields
    showFields name names

data GlobalSettings = GlobalSettings { getSettingsFolder :: String }

type FolderPath = String
type ContentType = String
type Url = String

data EventProvider a b = EventProvider {
    getModuleName :: String,
    getEvents :: a -> GlobalSettings -> Day -> (b -> Url) -> ExceptT String IO [TsEvent],
    getConfigType :: [ConfigDataInfo],
    getExtraData :: Maybe (a -> GlobalSettings -> b -> IO (Maybe (ContentType, ByteString)))
}

instance Show (EventProvider a b) where show = getModuleName

decodeVal :: FromJSON a => Value -> a
decodeVal value = case fromJSON value of
    Error msg -> error msg
    Success a -> a

eventProviderWrap :: (FromJSON a, ToJSON a, FromJSON b, ToJSON b) =>  EventProvider a b
                  -> EventProvider Value Value
eventProviderWrap (EventProvider innerGetModName innerGetEvents
        innerGetConfigType innerGetExtraData) = EventProvider
    {
        getModuleName = innerGetModName,
        getEvents = \a s d u -> innerGetEvents (decodeVal a) s d (u . toJSON),
        getConfigType = innerGetConfigType,
        getExtraData = innerGetExtraData >>= \decoder ->
            Just $ \cfg s k -> decoder (decodeVal cfg) s (decodeVal k)
    }
