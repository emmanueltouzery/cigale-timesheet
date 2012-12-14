module Email where

import Codec.Mbox
import Data.Time.Clock
import Data.Time.Calendar

sent_mbox = "C:\\Users\\emmanuelto\\AppData\\Roaming\\Thunderbird\\Profiles\\k5eh13s1.newprofile_windows7\\Mail\\mail.regulussoft.com\\Sent"

data Email = Email
	{
		subject :: String
	}

getEmails :: Day -> Day -> IO [Email]
getEmails fromDate toDate = do
	mbox <- parseMboxFile Backward sent_mbox
	return []
