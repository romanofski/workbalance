module Balance where

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
getWorkBalanceFromHamsterOutput :: Day -> Day -> String -> Double
getWorkBalanceFromHamsterOutput from to output = actual - expected
    where days = diffDays from to
          expected = expectedHours days
          actual = getWorkedHours $ cleanHamsterOutput output


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
