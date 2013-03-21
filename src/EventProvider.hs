module EventProvider where

import qualified Data.Text as T
import Data.Map
import Data.Time.Calendar
import Data.Aeson

import Event

-- data ConfigSpec = StringFieldSpec String
-- 			| SubElementArraySpec [ConfigSpec]
-- 			deriving (Show)
-- 
-- data Config = StringField String
-- 			| SubElementArray [Config]
-- 			deriving (Show)

data EventProvider a = EventProvider {
	getModuleName :: String,
	getEvents :: a -> Day -> Day -> IO [Event]
	
	-- i could derive the ConfigSpec from the data using
	-- template haskell or maybe sth like that:
	-- http://stackoverflow.com/questions/8457876/get-a-haskell-records-field-names-as-a-list-of-strings
	--getConfigRequirements :: ConfigSpec
}

instance Show (EventProvider a) where
	show (EventProvider modName _ ) = show modName

eventProviderWrap :: FromJSON a =>  EventProvider a -> EventProvider Value
eventProviderWrap (EventProvider innerGetModName innerGetEvents) = EventProvider
	{
		getModuleName = innerGetModName,
		getEvents = 	let
				decodeVal value = case fromJSON value of
						Error msg -> error msg
						Success a -> a
				in
				\value -> innerGetEvents (decodeVal value)
	}

