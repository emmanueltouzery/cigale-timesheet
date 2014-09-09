{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}
module EventProvider (thGetTypeDesc,
	GlobalSettings(GlobalSettings), EventProvider(..),
	eventProviderWrap, getSettingsFolder,
	ConfigDataType(..), ConfigDataInfo(..), FolderPath, ContentType) where

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import Data.ByteString (ByteString)
import Data.Aeson.TH (deriveJSON, deriveFromJSON, mkToJSON, defaultOptions)
import qualified FayAeson
import Control.Applicative
import Control.Error

import Event

data ConfigDataInfo = ConfigDataInfo
	{
		memberName :: String,
		memberType :: String
	} deriving (Eq, Show)
$(deriveLift ''ConfigDataInfo)
$(deriveFromJSON defaultOptions ''ConfigDataInfo)
instance ToJSON ConfigDataInfo where
     toJSON = FayAeson.addInstance "ConfigDataInfo" . $(mkToJSON defaultOptions ''ConfigDataInfo)

data ConfigDataType = ConfigDataType
	{
		dataName :: String,
		members :: [ConfigDataInfo]
	} deriving (Eq, Show)

$(deriveFromJSON defaultOptions ''ConfigDataType)
instance ToJSON ConfigDataType where
     toJSON = FayAeson.addInstance "ConfigDataType" . $(mkToJSON defaultOptions ''ConfigDataType)

formatTypeName :: Type -> String
formatTypeName (ConT x) = nameBase x
formatTypeName (AppT ListT x) = "[" ++ formatTypeName x ++ "]"
formatTypeName x@_ = "fallback " ++ show x

showField :: (Name,Type) -> ConfigDataInfo
showField nameType = ConfigDataInfo s typS 
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
	let names = map (\(name,_,typ) -> (name,typ)) fields
	showFields name names

data GlobalSettings = GlobalSettings {
	getSettingsFolder :: String
}

type FolderPath = String
type ContentType = String

data EventProvider a b = EventProvider {
	getModuleName :: String,
	getEvents :: a -> GlobalSettings -> Day -> EitherT String IO [Event],
	getConfigType :: [ConfigDataInfo],
	getExtraData :: Maybe (a -> GlobalSettings -> b -> IO (Maybe (ContentType, ByteString)))
}

instance Show (EventProvider a b) where show = getModuleName

decodeVal :: FromJSON a => Value -> a
decodeVal value = case fromJSON value of
	Error msg -> error msg
	Success a -> a

eventProviderWrap :: (FromJSON a, ToJSON a, FromJSON b, ToJSON b) =>  EventProvider a b -> EventProvider Value Value
eventProviderWrap (EventProvider innerGetModName innerGetEvents
		innerGetConfigType innerGetExtraData) = EventProvider
	{
		getModuleName = innerGetModName,
		getEvents = innerGetEvents . decodeVal,
		getConfigType = innerGetConfigType,
		getExtraData = innerGetExtraData >>= \decoder -> Just $ \cfg s k -> decoder (decodeVal cfg) s (decodeVal k)
	}
