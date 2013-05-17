{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module EventProvider (thGetTypeDesc,
	GlobalSettings(GlobalSettings), EventProvider(EventProvider),
	eventProviderWrap, getSettingsFolder,
	getEvents, getModuleName, getConfigType) where

import qualified Data.Text as T
--import Data.Map
import Data.Time.Calendar
import Data.Aeson
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Event

-- data ConfigSpec = StringFieldSpec String
-- 			| SubElementArraySpec [ConfigSpec]
-- 			deriving (Show)
-- 
-- data Config = StringField String
-- 			| SubElementArray [Config]
-- 			deriving (Show)

formatTypeName :: Type -> String
formatTypeName (ConT x) = nameBase x
formatTypeName (AppT ListT x) = "[" ++ (formatTypeName x) ++ "]"
formatTypeName x@_ = "fallback " ++ show x

showField :: (Name,Type) -> Q Exp
showField nameType = [|s ++ " :: " ++ typS |] where
	s = nameBase $ fst nameType
	typS = formatTypeName $ snd nameType

showFields :: Name -> [(Name, Type)] -> Q Exp
showFields name names = do
	exps <- sequence $ fmap showField names
	nameExp <- stringE $ nameBase name
	return $ ListE (nameExp:exps)

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
	getConfigType :: [[String]]
	
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

