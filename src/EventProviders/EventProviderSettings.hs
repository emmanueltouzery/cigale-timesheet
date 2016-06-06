-- I would rather have this in individual provider files,
-- but GHC stage restriction forces me to have like that.
module EventProviderSettings where

import EventProvider

gitUserName :: ConfigDataInfo
gitUserName = ConfigDataInfo "gitUser" "Git username" MtCombo DependsOnOthers

gitConfigDataType :: ConfigDataType
gitConfigDataType = ConfigDataType
    {
        dataName = "Git",
        members =
          [
              ConfigDataInfo "gitRepo" "Git repository path" MtFolderPath Standalone,
              gitUserName
          ]
    }

svnConfigDataType :: ConfigDataType
svnConfigDataType = ConfigDataType
    {
        dataName = "Svn",
        members =
          [
              ConfigDataInfo "svnUser" "Svn user" MtText Standalone,
              ConfigDataInfo "svnRepo" "Svn repository path" MtFolderPath Standalone
          ]
    }

hgConfigDataType :: ConfigDataType
hgConfigDataType = ConfigDataType
    {
        dataName = "Hg",
        members =
          [
              ConfigDataInfo "hgUser" "Mercurial user" MtText Standalone,
              ConfigDataInfo "hgRepo" "Mercurial repository path" MtFolderPath Standalone
          ]
    }

emailConfigDataType :: ConfigDataType
emailConfigDataType = ConfigDataType
    {
        dataName = "Email",
        members =
          [
              ConfigDataInfo "emailPath" "Path to the email mbox file" MtFilePath Standalone
          ]
    }

icalConfigDataType :: ConfigDataType
icalConfigDataType = ConfigDataType
    {
        dataName = "Ical",
        members =
          [
              ConfigDataInfo "icalUrl" "URL to the ical" MtText Standalone
          ]
    }

cfgItemSkypeUsername :: ConfigDataInfo
cfgItemSkypeUsername = ConfigDataInfo
    "skypeUsername" "Skype username" MtCombo Standalone

cfgItemSkypeConversations :: ConfigDataInfo
cfgItemSkypeConversations = ConfigDataInfo
    "skypeConversations" "Conversations to show" MtMultiChoice DependsOnOthers

skypeConfigDataType :: ConfigDataType
skypeConfigDataType = ConfigDataType
    {
        dataName = "Skype",
        members =
          [
              cfgItemSkypeUsername,
              cfgItemSkypeConversations
          ]
    }

redmineConfigDataType :: ConfigDataType
redmineConfigDataType = ConfigDataType
    {
        dataName = "Redmine",
        members =
          [
              ConfigDataInfo "redmineUrl" "Redmine URL" MtText Standalone,
              ConfigDataInfo "redmineUsername" "Username" MtText Standalone,
              ConfigDataInfo "redmineUserDisplay" "User display" MtText Standalone,
              ConfigDataInfo "redminePassword" "Password" MtPassword Standalone
          ]
    }
