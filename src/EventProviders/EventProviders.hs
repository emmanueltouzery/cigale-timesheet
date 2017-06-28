module EventProviders (
    plugins
) where

import Data.Aeson

import EventProvider
import Email
import Git
import Svn
import Hg
import Ical
import Skype
import Slack
import Redmine

plugins :: [EventProvider Value Value]
plugins = [
    eventProviderWrap getEmailProvider,
    eventProviderWrap getGitProvider,
    eventProviderWrap getSvnProvider,
    eventProviderWrap getHgProvider,
    eventProviderWrap getIcalProvider,
    eventProviderWrap getSkypeProvider,
    eventProviderWrap getSlackProvider,
    eventProviderWrap getRedmineProvider]
