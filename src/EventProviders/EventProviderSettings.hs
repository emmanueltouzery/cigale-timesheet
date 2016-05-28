-- I would rather have this in individual provider files,
-- but GHC stage restriction forces me to have like that.
module EventProviderSettings where

import EventProvider

gitConfigDataType :: ConfigDataType
gitConfigDataType = ConfigDataType
    {
        dataName = "Git",
        members =
          [
              ConfigDataInfo "gitUser" "Git username" MtText,
              ConfigDataInfo "gitRepo" "Git repository path" MtFolderPath
          ]
    }

svnConfigDataType :: ConfigDataType
svnConfigDataType = ConfigDataType
    {
        dataName = "Svn",
        members =
          [
              ConfigDataInfo "svnUser" "Svn user" MtText,
              ConfigDataInfo "svnRepo" "Svn repository path" MtFolderPath
          ]
    }

hgConfigDataType :: ConfigDataType
hgConfigDataType = ConfigDataType
    {
        dataName = "Hg",
        members =
          [
              ConfigDataInfo "hgUser" "Mercurial user" MtText,
              ConfigDataInfo "hgRepo" "Mercurial repository path" MtFolderPath
          ]
    }

emailConfigDataType :: ConfigDataType
emailConfigDataType = ConfigDataType
    {
        dataName = "Email",
        members =
          [
              ConfigDataInfo "emailPath" "Path to the email mbox file" MtFilePath
          ]
    }

icalConfigDataType :: ConfigDataType
icalConfigDataType = ConfigDataType
    {
        dataName = "Ical",
        members =
          [
              ConfigDataInfo "icalUrl" "URL to the ical" MtText
          ]
    }

cfgItemSkypeUsername :: ConfigDataInfo
cfgItemSkypeUsername = ConfigDataInfo "skypeUsername" "Skype username" MtCombo

skypeConfigDataType :: ConfigDataType
skypeConfigDataType = ConfigDataType
    {
        dataName = "Skype",
        members =
          [
              cfgItemSkypeUsername
          ]
    }

redmineConfigDataType :: ConfigDataType
redmineConfigDataType = ConfigDataType
    {
        dataName = "Redmine",
        members =
          [
              ConfigDataInfo "redmineUrl" "Redmine URL" MtText,
              ConfigDataInfo "redmineUsername" "Username" MtText,
              ConfigDataInfo "redmineUserDisplay" "User display" MtText,
              ConfigDataInfo "redminePassword" "Password" MtPassword
          ]
    }
