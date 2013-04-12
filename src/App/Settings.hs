module Settings where

import System.Directory

getSettingsFolder :: IO FilePath
getSettingsFolder = do
	home <- getHomeDirectory
	let result = home ++ "/.timesheet/"
	createDirectoryIfMissing False result
	return result
