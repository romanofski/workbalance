module Balance where

import System.Locale
import Data.Time.Format
import Data.Time.Calendar
import Data.List
import Data.Char


hoursPerDay :: Double
hoursPerDay = 38 / 5


-- | Calculates the work balance in hours
-- >>> getWorkBalanceFromHamsterOutput "2014-01-01" "2014-01-03" "\nFoo\nFoo: 15.0h, Frob frab: 0.2h\n\n"
-- 0.0
-- >>> getWorkBalanceFromHamsterOutput "2014-01-01" "2014-01-03" "\nFoo\nFoo: 16.0h, Frob frab: 0.1h\n\n"
-- 0.9000000000000021
--
getWorkBalanceFromHamsterOutput :: String -> String -> String -> Double
getWorkBalanceFromHamsterOutput from to output = actual - expected
    where days = getDiffDays from to
          expected = expectedHours $ days
          actual = getWorkedHours $ cleanHamsterOutput output

-- | Calculate day difference between two dates given as strings
-- formatted as %F
-- >>> getDiffDays "2014-01-01" "2014-01-02"
-- 1
-- >>> getDiffDays "2014-01-01" "2014-01-30"
-- 29
getDiffDays :: String -> String -> Integer
getDiffDays from to =
    case getDayToday from of
        Just f -> case getDayToday to of
            Just t -> diffDays t f
            Nothing -> 0
        Nothing -> 0

-- | Converts a datestring to a Day
-- >>> getDayToday "2014-04-01"
-- Just 2014-04-01
-- >>> getDayToday "20-412-asdf"
-- Nothing
getDayToday :: String -> Maybe Day
getDayToday xs = parseTime defaultTimeLocale "%F" xs


-- | Calculate work days for a given number of days
-- Note: This function is inefficient. I go over all days and divide
-- them up into weeks, filter out the weekends and then see how many
-- remaining days there are.
-- >>> workdays 14
-- 10
-- >>> workdays 30
-- 22
-- >>> workdays 58
-- 42
workdays :: Integer -> Integer
workdays n = toInteger $ length workdays
    where workdays = snd $ partition (<= 1) [x `mod` 7 | x <- [0..n + 1]]

-- | Calculate expected ours with the given workdays
-- >>> expectedHours 7
-- 38.0
-- >>> expectedHours 5 == expectedHours 7
-- True
-- >>> expectedHours 0
-- 0.0
-- >>> expectedHours (-1)
-- 0.0
expectedHours :: Integer -> Double
expectedHours n = (fromIntegral $ workdays n) * hoursPerDay

-- | Receives output from Hamster and extracts the total worked hours
-- >>> getWorkedHours []
-- 0.0
-- >>> getWorkedHours ["26.1","1"]
-- 27.1
getWorkedHours:: [String] -> Double
getWorkedHours xs = sum $ map read xs


-- | Helper to take a chunk of hamster output and return list of strings
-- with hours
-- >>> cleanHamsterOutput "\nFoo\nFoo: 26.1h, Frob frab: 1.4h\n\n"
-- ["26.1","1.4"]
cleanHamsterOutput :: String -> [String]
cleanHamsterOutput [] = []
cleanHamsterOutput raw = filter (not . null) $ fmap filterOutLetters (words substring)
    where substring = reverse (lines raw) !! 1
          filterOutLetters = filter (\x -> (x == '.' || isDigit x))
