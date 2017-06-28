{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Slack where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Control.Error
import Control.Monad.Trans
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Web.Slack as Slack
import Web.Slack.Common as Slack

import TsEvent
import qualified Util
import EventProvider
import EventProviderSettings

deriveConfigRecord slackConfigDataType
deriveJSON defaultOptions ''SlackConfigRecord

getSlackProvider :: EventProvider SlackConfigRecord ()
getSlackProvider = EventProvider
    {
        getModuleName = "Slack",
        getEvents     = getSlackMessages,
        getConfigType = members slackConfigDataType,
        getExtraData  = Nothing,
        fetchFieldCts = Nothing
    }

-- TODO bring upstream in slack-web?
historyRspGetMessages :: Slack.HistoryRsp -> Maybe [Slack.Message]
historyRspGetMessages (HistoryRsp True messages _) = Just messages
historyRspGetMessages _ = Nothing

getSlackMessages :: SlackConfigRecord -> GlobalSettings -> Day -> (() -> Url)
                 -> ExceptT String IO [TsEvent]
getSlackMessages (SlackConfigRecord token) _ date _ = do
    manager <- lift Slack.mkManager
    -- groups <- Slack.run manager (Slack.groupsList token mkListReq)
    historyRsp <- lift $ Slack.run manager $ Slack.historyFetchAll token groupsHistory
               "G3PJZTADV" 50
               (mkSlackTimestamp $ UTCTime date 0)
               (mkSlackTimestamp $ UTCTime (addDays 1 date) 0)
    lift $ print historyRsp
    return $ fromMaybe []
        (sequence [messagesToEvent "channel name" =<< historyRspGetMessages =<< hush historyRsp])

messagesToEvent :: Text -> [Slack.Message] -> Maybe TsEvent
messagesToEvent _ [] = Nothing
messagesToEvent channel msgs@(msg:_) = Just TsEvent
    {
        pluginName   = getModuleName getSlackProvider,
        eventIcon    = "glyphicons-245-conversation",
        eventDate    = slackTimestampTime (messageTs msg),
        desc         = channel,
        extraInfo    = "",
        fullContents = Just (formatMessages msgs)
    }

formatMessages :: [Slack.Message] -> Text
formatMessages msgs = T.intercalate "<br/>" (formatMessage <$> msgs)
    where formatMessage Message{..} = Util.linksForceNewWindow $
              T.concat ["<b>", T.pack $ show messageUser, ":<b> ", messageText]
