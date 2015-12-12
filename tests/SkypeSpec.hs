{-# LANGUAGE OverloadedStrings #-}

module SkypeSpec where

import Test.Hspec
import Test.HUnit
import Data.Time.Calendar
import Data.Time.Clock

import Skype

spec :: Spec
spec = do
    splitWhenNeeded
    noSplitIfNotNeeded
    splitFourElts

-- http://therning.org/magnus/archives/389
skypeMinIntervalToSplitChatsSecondsD :: DiffTime
skypeMinIntervalToSplitChatsSecondsD = fromInteger $ floor skypeMinIntervalToSplitChatsSeconds

splitWhenNeeded :: Spec
splitWhenNeeded = it "splits when needed" $ do
    let day = fromGregorian 2012 1 1
    let t1 = UTCTime day 0
    let t2 = UTCTime day (skypeMinIntervalToSplitChatsSecondsD+1)
    let source = [[ChatRecord "" t1 "", ChatRecord "" t2 ""]]
    let expected = [[ChatRecord "" t1 ""], [ChatRecord "" t2 ""]]
    assertEqual "doesn't match" expected (splitFarawayChats source)

noSplitIfNotNeeded :: Spec
noSplitIfNotNeeded = it "no split if not needed" $ do
                let day = fromGregorian 2012 1 1
                let t1 = UTCTime day 0
                let t2 = UTCTime day (skypeMinIntervalToSplitChatsSecondsD-1)
                let source = [[ChatRecord "" t1 "", ChatRecord "" t2 ""]]
                let expected = [[ChatRecord "" t1 "", ChatRecord "" t2 ""]]
                assertEqual "doesn't match" expected (splitFarawayChats source)

splitFourElts :: Spec
splitFourElts = it "splits four elements" $ do
                let day = fromGregorian 2012 1 1
                let t1 = UTCTime day 0
                let t2 = UTCTime day (skypeMinIntervalToSplitChatsSecondsD+1)
                let t3 = UTCTime day (skypeMinIntervalToSplitChatsSecondsD*2+2)
                let t4 = UTCTime day (skypeMinIntervalToSplitChatsSecondsD*2+3)
                let source = [[
                            ChatRecord "" t1 "", ChatRecord "" t2 "",
                            ChatRecord "" t3 "", ChatRecord "" t4 ""
                        ]]
                let expected = [[ChatRecord "" t1 ""], [ChatRecord "" t2 ""],
                        [ChatRecord "" t3 "", ChatRecord "" t4 ""]]
                assertEqual "doesn't match" expected (splitFarawayChats source)
