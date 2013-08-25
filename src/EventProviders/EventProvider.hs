{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}
module EventProvider (thGetTypeDesc,
	GlobalSettings(GlobalSettings), EventProvider(EventProvider),
	eventProviderWrap, getSettingsFolder,
	getEvents, getModuleName, getConfigType,
	ConfigDataType(..), ConfigDataInfo(..)) where

import qualified Data.Text as T
--import Data.Map
import Data.Time.Calendar
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import Data.Aeson.TH (deriveJSON, deriveFromJSON, mkToJSON)
import qualified FayAeson

import Event

data ConfigDataInfo = ConfigDataInfo
	{
		memberName :: String,
		memberType :: String
	} deriving (Eq, Show)
$(deriveLift ''ConfigDataInfo)
$(deriveFromJSON id ''ConfigDataInfo)
instance ToJSON ConfigDataInfo where
     toJSON = (FayAeson.addInstance "ConfigDataInfo") . $(mkToJSON id ''ConfigDataInfo)

data ConfigDataType = ConfigDataType
	{
		dataName :: String,
		members :: [ConfigDataInfo]
	} deriving (Eq, Show)

$(deriveFromJSON id ''ConfigDataType)
instance ToJSON ConfigDataType where
     toJSON = (FayAeson.addInstance "ConfigDataType") . $(mkToJSON id ''ConfigDataType)

formatTypeName :: Type -> String
formatTypeName (ConT x) = nameBase x
formatTypeName (AppT ListT x) = "[" ++ (formatTypeName x) ++ "]"
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

data EventProvider a = EventProvider {
	getModuleName :: String,
	getEvents :: a -> GlobalSettings -> Day -> IO [Event],
	getConfigType :: [ConfigDataInfo]
	
	-- i could derive the ConfigSpec from the data using
	-- template haskell or maybe sth like that:
	-- http://stackoverflow.com/questions/8457876/get-a-haskell-records-field-names-as-a-list-of-strings
	--getConfigRequirements :: ConfigSpec
}

instance Show (EventProvider a) where
	show (EventProvider modName _ _) = show modName

eventProviderWrap :: FromJSON a =>  EventProvider a -> EventProvider Value
eventProviderWrap (EventProvider innerGetModName innerGetEvents innerGetConfigType) = EventProvider
	{
		getModuleName = innerGetModName,
		getEvents = 	let
				decodeVal value = case fromJSON value of
						Error msg -> error msg
						Success a -> a
				in
				\value -> innerGetEvents (decodeVal value),
		getConfigType = innerGetConfigType
	}

