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
import Text.Printf (printf)
default (LT.Text)

startDate :: Day
startDate = fromGregorian 2015 1 5

toString :: LT.Text -> String
toString xs = LT.unpack xs

toInternalText :: String -> T.Text
toInternalText xs = LT.toStrict $ LT.pack xs

yesterDay :: UTCTime -> Day
yesterDay x = addDays (-1) (utctDay x)

-- | return the raw output by invoking 'hamster search'
-- The function expects two dates: from and to date which define the
-- range hamster is looking for entries.
getHamsterOutput :: Day -> Day -> Sh (T.Text)
getHamsterOutput x y = do
        let fromDate = formatTime defaultTimeLocale (toString "%F") x
        let toDate = formatTime defaultTimeLocale (toString "%F") y
        output <- run "hamster" ["search", "", (toInternalText fromDate), (toInternalText toDate)]
        return output

main :: IO ()
main = shelly $ silently $ do
    nowTime <- liftIO $ getCurrentTime
    let yD = yesterDay nowTime

    output <- getHamsterOutput startDate yD
    let raw = LT.unpack (LT.fromStrict output)
    let balance = getWorkBalanceFromHamsterOutput startDate yD raw
    liftIO $ printf "W: %.2f" balance
