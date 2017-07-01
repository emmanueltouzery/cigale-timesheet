{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module Slack where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe
import Control.Error
import Control.Monad.Trans
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Servant.Client
import Web.Slack as Slack
import Web.Slack.Common as Slack
import Web.Slack.Group as Slack

import TsEvent
import qualified Util
import EventProvider
import EventProviderSettings

messagesFetchBy :: Int
messagesFetchBy = 100

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

reduceErrors :: Either ServantError (Slack.Response a) -> Either String a
reduceErrors (Left er) = Left (show er)
reduceErrors (Right (Left er)) = Left (show er)
reduceErrors (Right (Right x)) = Right x

getSlackMessages :: SlackConfigRecord -> GlobalSettings -> Day -> (() -> Url)
                 -> ExceptT String IO [TsEvent]
getSlackMessages (SlackConfigRecord token) _ date _ = do
    manager <- lift Slack.mkManager
    let slackRun = fmap reduceErrors . Slack.run manager
    groups <- lift $ slackRun (Slack.groupsList token)
    case groups of
      Left er    -> throwE er
      Right grps -> ExceptT $ fmap (fmap catMaybes . sequence) <$>
          traverse (fetchMessages slackRun date $ Slack.historyFetchAll token groupsHistory) $ Slack.groupId <$> listRspGroups grps

fetchMessages :: (ClientM (Response HistoryRsp) -> IO (Either String HistoryRsp))
              -> Day
              -> (Text -> Int -> SlackTimestamp -> SlackTimestamp -> ClientM (Slack.Response HistoryRsp))
              -> Text
              -> IO (Either String (Maybe TsEvent))
fetchMessages slackRun date fetcher item = do
    historyRsp <- slackRun $ fetcher
               item messagesFetchBy
               (mkSlackTimestamp $ UTCTime date 0)
               (mkSlackTimestamp $ UTCTime (addDays 1 date) 0)
    return $ fmapR (messagesToEvent "channel name" . historyRspMessages) historyRsp

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
              T.concat ["<b>", T.pack $ show messageUser, ":</b> ", messageText]
