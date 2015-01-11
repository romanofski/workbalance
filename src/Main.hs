#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Time
import System.Locale
import Balance
default (LT.Text)

startDate :: Day
startDate = fromGregorian 2015 1 1

toString :: LT.Text -> String
toString xs = LT.unpack xs

toInternalText :: String -> T.Text
toInternalText xs = LT.toStrict $ LT.pack xs

main :: IO ()
main = shelly $ do
    nowTime <- liftIO $ getCurrentTime
    let toDate = formatTime defaultTimeLocale (toString "%F") nowTime
    let fromDate = formatTime defaultTimeLocale (toString "%F") startDate

    output <- run "hamster" ["search", "", (toInternalText fromDate), (toInternalText toDate)]

    let raw = LT.unpack (LT.fromStrict output)
    let balance = getWorkBalanceFromHamsterOutput startDate (utctDay nowTime) raw
    liftIO $ putStrLn (show balance)
