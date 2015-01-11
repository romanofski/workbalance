#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import qualified Data.Text.Lazy as LT
import Balance
default (LT.Text)

startDate = "2015-01-01"

main :: IO ()
main = shelly $ verbosely $ do
    now <- run "date" ["+'%F'"]
    output <- run "hamster" ["search", "''", startDate, "2015-01-09"]
    let raw = LT.unpack (LT.fromStrict output)
    let balance = getWorkBalanceFromHamsterOutput (LT.unpack $ LT.fromStrict startDate) (LT.unpack $ LT.fromStrict now) raw
    liftIO $ putStrLn (show balance)
