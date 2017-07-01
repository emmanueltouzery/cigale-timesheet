{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, RankNTypes #-}

module Slack where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe
import Data.Map as Map hiding (filter)
import Control.Error
import Control.Arrow ((&&&))
import Control.Monad.Trans
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Servant.Client
import Web.Slack as Slack
import Web.Slack.Common as Slack
import Web.Slack.User as Slack
import Web.Slack.Group as Slack
import Web.Slack.Channel as Slack
import Web.Slack.Im as Slack

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
    users <- ExceptT $ slackRun (Slack.usersList token)
    let userIdToName = Map.fromList $ (userId &&& userName) <$> listRspMembers users
    let getUserName = fromMaybe "Unknown user" . flip Map.lookup userIdToName
    let getMsgs = getMessages slackRun token date getUserName
    groupEvts <- getMsgs Slack.groupsList listRspGroups groupsHistory groupId groupName
    channelEvts <- getMsgs (`Slack.channelsList` mkListReq) listRspChannels channelsHistory channelId channelName
    imsEvts <- getMsgs Slack.imList listRspIms imHistory imId (getUserName . imUser)
    return (groupEvts ++ channelEvts ++ imsEvts)

type SlackRun = forall a. (ClientM (Response a) -> IO (Either String a))

getMessages :: SlackRun
            -> Text
            -> Day
            -> (UserId -> Text)
            -> (Text -> ClientM (Response chatlist))
            -> (chatlist -> [chat])
            -> (Text -> HistoryReq -> ClientM (Response HistoryRsp))
            -> (chat -> Text)
            -> (chat -> Text)
            -> ExceptT String IO [TsEvent]
getMessages slackRun token date getUserName getList listGetChats getHistory getChatId getChatName = do
    groups <- lift $ slackRun (getList token)
    case groups of
      Left er    -> throwE er
      Right grps -> ExceptT $ fmap (fmap catMaybes . sequence) <$>
          traverse (fetchMessages token slackRun date getUserName getHistory getChatId getChatName) $ listGetChats grps

fetchMessages :: Text
              -> SlackRun
              -> Day
              -> (UserId -> Text)
              -> (Text -> HistoryReq -> ClientM (Response HistoryRsp))
              -> (a -> Text)
              -> (a -> Text)
              -> a
              -> IO (Either String (Maybe TsEvent))
fetchMessages token slackRun date getUserName fetcher convId convName item = do
    historyRsp <- slackRun $ Slack.historyFetchAll token fetcher
               (convId item) messagesFetchBy
               (mkSlackTimestamp $ UTCTime date 0)
               (mkSlackTimestamp $ UTCTime (addDays 1 date) 0)
    return $ fmapR (messagesToEvent (convName item) getUserName . historyRspMessages) historyRsp

messagesToEvent :: Text -> (UserId -> Text) -> [Slack.Message] -> Maybe TsEvent
messagesToEvent _ _ [] = Nothing
messagesToEvent channel getUserName msgs@(msg:_) = Just TsEvent
    {
        pluginName   = getModuleName getSlackProvider,
        eventIcon    = "glyphicons-245-conversation",
        eventDate    = slackTimestampTime (messageTs msg),
        desc         = channel,
        extraInfo    = "",
        fullContents = Just (formatMessages getUserName msgs)
    }

formatMessages :: (UserId -> Text) -> [Slack.Message] -> Text
formatMessages getUserName msgs = T.intercalate "<br/>" (formatMessage <$> reverse msgsWithUser)
    where
      msgsWithUser = filter (isJust . messageUser) msgs
      formatMessage Message{..} = Util.linksForceNewWindow $
          T.concat ["<b>", getUserName (fromJust messageUser), ":</b> ", messageText]
