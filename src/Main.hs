#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Time.Calendar (addDays)
import Data.Time
import System.Locale
import Balance
default (LT.Text)

startDate :: Day
startDate = fromGregorian 2015 1 5

toString :: LT.Text -> String
toString xs = LT.unpack xs

toInternalText :: String -> T.Text
toInternalText xs = LT.toStrict $ LT.pack xs

main :: IO ()
main = shelly $ verbosely $ do
    nowTime <- liftIO $ getCurrentTime
    let yesterDay = addDays (-1) (utctDay nowTime)
    let toDate = formatTime defaultTimeLocale (toString "%F") yesterDay
    let fromDate = formatTime defaultTimeLocale (toString "%F") startDate

    output <- run "hamster" ["search", "", (toInternalText fromDate), (toInternalText toDate)]

    let raw = LT.unpack (LT.fromStrict output)
    let balance = getWorkBalanceFromHamsterOutput startDate yesterDay raw
    liftIO $ putStrLn (show balance)
