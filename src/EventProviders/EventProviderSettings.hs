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
              ConfigDataInfo "gitUser" MtText,
              ConfigDataInfo "gitRepo" MtFolderPath
          ]
    }

svnConfigDataType :: ConfigDataType
svnConfigDataType = ConfigDataType
    {
        dataName = "Svn",
        members =
          [
              ConfigDataInfo "svnUser" MtText,
              ConfigDataInfo "svnRepo" MtFolderPath
          ]
    }

hgConfigDataType :: ConfigDataType
hgConfigDataType = ConfigDataType
    {
        dataName = "Hg",
        members =
          [
              ConfigDataInfo "hgUser" MtText,
              ConfigDataInfo "hgRepo" MtFolderPath
          ]
    }

emailConfigDataType :: ConfigDataType
emailConfigDataType = ConfigDataType
    {
        dataName = "Email",
        members =
          [
              ConfigDataInfo "emailPath" MtFilePath
          ]
    }

icalConfigDataType :: ConfigDataType
icalConfigDataType = ConfigDataType
    {
        dataName = "Ical",
        members =
          [
              ConfigDataInfo "icalUrl" MtText
          ]
    }

skypeConfigDataType :: ConfigDataType
skypeConfigDataType = ConfigDataType
    {
        dataName = "Skype",
        members =
          [
              ConfigDataInfo "skypeUsername" MtText
          ]
    }

redmineConfigDataType :: ConfigDataType
redmineConfigDataType = ConfigDataType
    {
        dataName = "Redmine",
        members =
          [
              ConfigDataInfo "redmineUrl" MtText,
              ConfigDataInfo "redmineUsername" MtText,
              ConfigDataInfo "redmineUserDisplay" MtText,
              ConfigDataInfo "redminePassword" MtPassword
          ]
    }
