{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, RankNTypes #-}

module Slack where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Maybe
import Data.Monoid
import Control.Error
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Morph
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Web.Slack as Slack
import Web.Slack.Common as Slack
import Web.Slack.Group as Slack
import Web.Slack.Channel as Slack
import Web.Slack.Im as Slack
import Web.Slack.MessageParser as Slack

import TsEvent
import qualified Util
import EventProvider
import EventProviderSettings

messagesFetchBy :: Int
messagesFetchBy = 100

deriveConfigRecord slackConfigDataType
deriveJSON defaultOptions ''SlackConfigRecord

type CigaleSlack a = ExceptT SlackClientError (ReaderT SlackConfig IO) a
type SlackResponse a = ReaderT SlackConfig IO (Response a)

getSlackProvider :: EventProvider SlackConfigRecord ()
getSlackProvider = EventProvider
    {
        getModuleName = "Slack",
        getEvents     = getSlackMessages,
        getConfigType = members slackConfigDataType,
        getExtraData  = Nothing,
        fetchFieldCts = Nothing
    }

getSlackMessages :: SlackConfigRecord -> GlobalSettings -> Day -> (() -> Url)
                 -> ExceptT String IO [TsEvent]
getSlackMessages (SlackConfigRecord token) _ date _ = do
    slackConfig <- lift (Slack.mkSlackConfig token)
    fmapLT show $ hoist (flip runReaderT slackConfig) (getSlackMessages' date)

getSlackMessages' :: Day -> CigaleSlack [TsEvent]
getSlackMessages' date = do
    users <- ExceptT Slack.usersList
    let getUserName = Slack.getUserDesc unUserId users
    let getMsgs = getMessages date getUserName
    groupEvts <- getMsgs Slack.groupsList listRspGroups groupsHistory groupId (niceGroupName getUserName)
    channelEvts <- getMsgs (Slack.channelsList mkListReq) listRspChannels
                           channelsHistory channelId (("#" <>) . channelName)
    imEvts <- getMsgs Slack.imList listRspIms imHistory imId (getUserName . imUser)
    -- I don't think I need to fetch MPIMs for now. If I do, I get overlaps with groups.
    -- https://api.slack.com/types/group
    -- "For compatibility with older clients, mpims can appear as private
    -- channels unless rtm.start is called with mpim_aware=1."
    -- https://api.slack.com/types/mpim
    -- "For compatibility with older clients, mpims can appear as groups
    -- unless rtm.start is called with mpim_aware=1."
    -- mpimEvts <- getMsgs Slack.mpimList listRspGroups mpimHistory groupId groupName
    return (groupEvts ++ channelEvts ++ imEvts)

niceGroupName :: (UserId -> Text) -> Group -> Text
niceGroupName getUserName Group{..} = if groupIsMpim
    then T.intercalate ", " $ getUserName <$> groupMembers
    else "🔒" <> groupName

getMessages
    :: Day
    -> (UserId -> Text)
    -> SlackResponse chatlist
    -> (chatlist -> [chat])
    -> (HistoryReq -> SlackResponse HistoryRsp)
    -> (chat -> Text)
    -> (chat -> Text)
    -> CigaleSlack [TsEvent]
getMessages date getUserName getList listGetChats getHistory getChatId getChatName = do
    chats <- listGetChats <$> ExceptT getList
    let fetchChat = fetchMessages date getUserName getHistory getChatId getChatName
    catMaybes <$> traverse fetchChat chats

fetchMessages
    :: Day
    -> (UserId -> Text)
    -> (HistoryReq -> SlackResponse HistoryRsp)
    -> (a -> Text)
    -> (a -> Text)
    -> a
    -> CigaleSlack (Maybe TsEvent)
fetchMessages date getUserName fetcher convId convName item = do
    tz <- liftIO getCurrentTimeZone
    let startOfDayUTC day = mkSlackTimestamp $ localTimeToUTC tz $
            LocalTime day $ TimeOfDay 0 0 0
    historyRsp <- ExceptT $ Slack.historyFetchAll fetcher
               (convId item) messagesFetchBy
               (startOfDayUTC date)
               (startOfDayUTC $ addDays 1 date)
    return $ messagesToEvent (convName item) getUserName $ historyRspMessages historyRsp

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
          T.concat ["<b>", getUserName (fromJust messageUser), ":</b> ", messageCts messageText]
      messageCts = Slack.messageToHtml getUserName
