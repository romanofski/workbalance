module Balance where

import Data.Time.Format
import Data.Time.Calendar
import Data.List
import Data.Char
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT


hoursPerDay :: Double
hoursPerDay = 38 / 5


-- | Calculates the work balance in hours
-- >>> getWorkBalanceFromHamsterOutput (fromGregorian 2020 11 16) (fromGregorian 2020 11 18) (T.pack "Total: 15h 12min\n")
-- Right 0.0
--
getWorkBalanceFromHamsterOutput :: Day -> Day -> T.Text -> Either String Double
getWorkBalanceFromHamsterOutput from to output = (\actual -> actual - expected)
                                                 <$> getWorkedHours output
    where days = diffDays to from
          expected :: Double
          expected = expectedHours days


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
workdays n = toInteger $ length go
    where go = snd $ partition (\x -> x == 0 || x == 6) [x `mod` 7 | x <- [0..n]]

-- | Calculate expected hours with the given workdays
-- >>> expectedHours 7
-- 38.0
-- >>> expectedHours 0
-- 0.0
-- >>> expectedHours (-1)
-- 0.0
expectedHours :: Integer -> Double
expectedHours n = fromIntegral (workdays n) * hoursPerDay

-- | Receives output from Hamster and extracts the total worked hours
-- >>> getWorkedHours (T.pack "Total: 15h 21min\n")
-- Right 15.35
-- >>> getWorkedHours (T.pack "Frob asdf\n\n")
-- Left "not enough input"
getWorkedHours:: T.Text -> Either String Double
getWorkedHours = AT.parseOnly parseHoursMinutes

parseHoursMinutes :: AT.Parser Double
parseHoursMinutes = do
  hours <- AT.skipWhile (not . isDigit) *> AT.double
  minutes <- AT.skipWhile (not . isDigit) *> AT.double
  pure (hours + (minutes * 1 / 60))
