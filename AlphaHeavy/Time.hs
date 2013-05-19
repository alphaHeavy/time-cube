{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AlphaHeavy.Time (
    Day(..)
  , Hour(..)
  , Minute(..)
  , Second(..)
  , Millis(..)
  , Month(..)
  , MonthOfYear(..)
  , Week(..)
  , Year(..)
  , Century(..)
  , Era(..)
  , Nanos(..)
  , DayOfWeek(..)

  , Calendar(..)
  , TimeZone(..)
  , Time(..)
  , DateTime
  , DateTimeMath(..)

  , UnixDate(..)
  , UnixTime(..)
  , UnixDateTime(..)
  , UnixDateTimeNanos(..)

  , UTCDate
  , UTCTime
  , UTCDateTime
  , UTCDateTimeNanos

  , UTCDateTimeStruct(..)
  , UTCDateStruct(..)

  , DateComponent(..)

  , addDateTime
  , deltaInMillis
  , floorToInterval
  , getTimePart
  , getDatePart
  , asDateTime

  , createTime
  , createDateTime
  , createUTCDateTime
  , createLocalDateTime
  , createDateTimeNanos
  , createUTCDateTimeNanos
  , createDate
  , createDate'

  , haskellLocalTime
  , haskellLocalTime'
  , haskellUTCTime
  , haskellUTCTime'
  , haskellDate

  , fromTimezone
  , fromTimezoneToUTC'
  , toTimezone
  , fromUTCToTimezone'
  , loadTimeZone

  , HasEpoch(..)
  , HasEra(..)
  , HasCentury(..)
  , HasYear(..)
  , HasYearOfEra(..)
  , HasYearOfCentury(..)
  , HasMonth(..)
  , HasDay(..)
  , HasDayOfYear(..)
  , HasDayOfWeek(..)
  , HasHour(..)
  , HasMinute(..)
  , HasMinuteOfDay(..)
  , HasSecond(..)
  , HasSecondOfDay(..)
  , HasMillis(..)
  , HasMillisOfDay(..)
  , HasNanos(..)

  , HasUtc(..)

  , HasDateStruct(..)
  , HasDateTimeStruct(..)

  , getCurrentDateTime
  , getCurrentDateTimeNanos
  , javaDateTimeToUTCDateTime
  , dateTimeAsString
  , dateTimeAsString'
  , today
  , today'

  , isWeekDay
  , isLeapYear
  , daysInMonth

  , CalculateDuration(..)
  , DurationResult(..)
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Convertible
import Data.Int
import Data.Data
import Data.Function (on)
import Data.Maybe (fromJust)
import qualified Data.Time as HT
import Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile)
import Data.Time.LocalTime.TimeZone.Series (localTimeToUTC',TimeZoneSeries, utcToLocalTime')
import Foreign.C.Types (CTime, CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import GHC.Generics (Generic)

import AlphaHeavy.Time.Calendar
import AlphaHeavy.Time.Native
import AlphaHeavy.Time.TM
import AlphaHeavy.Time.Types
import AlphaHeavy.Time.Zones

isWeekDay :: DayOfWeek 'ISO8601 -> Bool
isWeekDay Saturday = False
isWeekDay Sunday = False
isWeekDay _ = True

type Date x = (HasYear x, HasMonth x, HasDay x)
type Time x = (HasHour x, HasMinute x, HasSecond x)
type DateTime x = (Date x, Time x)

-- http://joda-time.sourceforge.net/field.html

type UTCTime = UnixTime 'UTC 'ISO8601
type UTCDate = UnixDate 'UTC 'ISO8601
type UTCDateTime = UnixDateTime 'UTC 'ISO8601
type UTCDateTimeNanos = UnixDateTimeNanos 'UTC 'ISO8601

instance Bounded (UnixDate tz cal) where
  minBound = UnixDate 0
  maxBound = UnixDate maxBound

instance Enum (UnixDate tz cal) where
  succ (UnixDate v) | v <= maxBound - 86400 = UnixDate $ v + 86400
                    | otherwise = error "UnixDate value out of range"

  pred (UnixDate v) | v >= 86400 = UnixDate $ v - 86400
                    | otherwise = error "UnixDate value out of range"

  toEnum v | v < 0 = error "UnixDate cannot represent dates before Jan 1, 1970"
           | otherwise = UnixDate $ (fromIntegral v `div` 86400) * 86400
  fromEnum (UnixDate v) = fromIntegral v

  enumFrom v = v:enumFrom (succ v)
  enumFromTo i1 i2 = enumFromTo' (norm i1) (norm i2) where
    norm (UnixDate dt) = UnixDate $ (dt `div` 86400) * 86400
    enumFromTo' v1 v2 | v1 == v2 = [v1]
                      | v1 < v2 = v1:enumFromTo (succ v1) v2
                      | v1 > v2 = v1:enumFromTo (pred v1) v2

instance Show (UnixDate tz cal) where
  show dt = printf "%04d-%02d-%02d" (getYears $ dt ^. year) (getMonths $ dt ^. month) (getDays $ dt ^. day)

instance Bounded (UnixDateTime tz cal) where
  minBound = UnixDateTime 0
  maxBound = UnixDateTime maxBound

instance Enum (UnixDateTime tz cal) where
  succ (UnixDateTime v) = UnixDateTime $ succ v
  pred (UnixDateTime v) | v > 0 = UnixDateTime $ pred v
                        | otherwise = error "UnixDateTime value out of range"

  toEnum v | v < 0 = error "UnixDateTime cannot represent dates before Jan 1, 1970"
           | otherwise = UnixDateTime $ fromIntegral v
  fromEnum (UnixDateTime v) = fromIntegral v

  enumFrom v = [dt | dt <- v:(enumFrom $ succ v)]
  enumFromTo i1 i2 | i1 == i2 = [i1]
                   | i1 < i2 = i1:enumFromTo (succ i1) i2
                   | i1 > i2 = i1:enumFromTo (pred i1) i2

class CalculateDuration a where
  data DurationResult a :: *

  calculateDuration :: a -> a -> DurationResult a

instance CalculateDuration (UnixDateTime tz cal) where
  newtype DurationResult (UnixDateTime tz cal) = Duration Int64
    deriving (Ord,Show,Num,Eq,NFData,Typeable,Generic)

  calculateDuration (UnixDateTime dt1) (UnixDateTime dt2) =
    Duration (dt1 - dt2)

instance CalculateDuration (UnixDateTimeNanos tz cal) where
  newtype DurationResult (UnixDateTimeNanos tz cal) = DurationNanos (Int64,Int32)
    deriving (Ord,Show,Eq,NFData,Generic)

  calculateDuration (UnixDateTimeNanos (dt1, ns1)) (UnixDateTimeNanos (dt2, ns2)) =
    DurationNanos (dt1 - dt2, ns1 - ns2)

data DateComponent a cal where
  YearComponent   :: Year   cal -> DateComponent Year   cal
  MonthComponent  :: Month  cal -> DateComponent Month  cal
  DayComponent    :: Day    cal -> DateComponent Day    cal
  HourComponent   :: Hour   cal -> DateComponent Hour   cal
  MinuteComponent :: Minute cal -> DateComponent Minute cal
  SecondComponent :: Second cal -> DateComponent Second cal
  MillisComponent :: Millis cal -> DateComponent Millis cal
  NanosComponent  :: Nanos  cal -> DateComponent Nanos  cal

data UTCDateTimeStruct cal = UTCDateTimeStruct
  { dt_year   :: Year cal
  , dt_month  :: Month cal
  , dt_day    :: Day cal
  , dt_hour   :: Hour cal
  , dt_minute :: Minute cal
  , dt_second :: Second cal
  , dt_millis :: Millis cal
  } deriving (Eq, Ord, Show)

instance NFData (UTCDateTimeStruct cal) where
  rnf UTCDateTimeStruct{..} =
    rnf dt_millis `seq`
    rnf dt_second `seq`
    rnf dt_minute `seq`
    rnf dt_hour   `seq`
    rnf dt_day    `seq`
    rnf dt_month  `seq`
    rnf dt_year   `seq` ()

data UTCDateStruct cal = UTCDateStruct
  { d_year   :: Year cal
  , d_month  :: Month cal
  , d_day    :: Day cal
  } deriving (Eq, Ord, Show)

instance NFData (UTCDateStruct cal) where
  rnf UTCDateStruct{..} =
    rnf d_day   `seq`
    rnf d_month `seq`
    rnf d_year  `seq` ()

instance Show (UnixDateTime tz cal) where
  show = dateTimeAsString' False

instance Show (UnixDateTimeNanos tz cal) where
  show = dateTimeAsString' True

dateTimeAsString :: HasDateTimeStruct dt => dt cal -> String
dateTimeAsString = dateTimeAsString' True

dateTimeAsString' :: HasDateTimeStruct dt => Bool -> dt cal -> String
dateTimeAsString' dispMs dt
  | dispMs    = printf "%04d-%02d-%02d %02d:%02d:%02d.%03d" y mon d h mi s ms
  | otherwise = printf "%04d-%02d-%02d %02d:%02d:%02d"      y mon d h mi s
  where UTCDateTimeStruct
          { dt_year   = Year y
          , dt_month  = Month mon
          , dt_day    = Day d
          , dt_hour   = Hour h
          , dt_minute = Minute mi
          , dt_second = Second s
          , dt_millis = Millis ms
          } = view dateTimeStruct dt

getTimePart :: Time (x tz) => (x tz) cal -> UnixTime tz cal
getTimePart dt = UnixTime 0 & hour .~ view hour dt & minute .~ view minute dt & second .~ view second dt

getDatePart :: Date (x tz) => (x tz) cal -> UnixDate tz cal
getDatePart dt = UnixDate 0 & year .~ view year dt & month .~ view month dt & day .~ view day dt

class DateTimeMath (a :: Calendar -> *) (b :: Calendar -> *) where
  datePlus :: a cal -> b cal -> a cal

instance DateTimeMath (UnixDate tz) Day where
   UnixDate base `datePlus` Day day = UnixDate $ base + fromIntegral (day * 86400)

instance DateTimeMath (UnixDateTime tz) Second where
  UnixDateTime base `datePlus` Second sec = UnixDateTime $ base + fromIntegral sec

instance DateTimeMath (UnixDateTime tz) Minute where
  UnixDateTime base `datePlus` Minute m = UnixDateTime $ base + fromIntegral (m * 60)

instance DateTimeMath (UnixDateTime tz) Hour where
  UnixDateTime base `datePlus` Hour hr = UnixDateTime $ base + fromIntegral (hr * 60 * 60)

instance DateTimeMath (UnixDateTime tz) Day where
  UnixDateTime base `datePlus` Day d = UnixDateTime $ base + fromIntegral (d * 24 * 60 * 60)

instance DateTimeMath (UnixDateTimeNanos tz) Nanos where
  UnixDateTimeNanos (base, base_ns) `datePlus` Nanos add_ns = UnixDateTimeNanos (base + add_sec, (fromIntegral new_ns)) where
    (add_sec, new_ns) = ((fromIntegral base_ns) + add_ns) `divMod` 1000000000

instance DateTimeMath (UnixDateTimeNanos tz) Millis where
  UnixDateTimeNanos (base, base_ns) `datePlus` Millis add_ms = UnixDateTimeNanos (base + add_sec, (fromIntegral new_ns)) where
    -- The millis that come in are an Int64, so we do the math to split total millis into total seconds and millis since last second in Int64, then then truncate the remainder down to Int32
    (add_sec, new_ns) = ((fromIntegral base_ns) + add_ms * 1000000) `divMod` 1000000000

instance DateTimeMath (UnixDateTimeNanos tz) Second where
  UnixDateTimeNanos (base, ns) `datePlus` Second sec = UnixDateTimeNanos (base + fromIntegral sec, ns)

instance DateTimeMath (UnixDateTimeNanos tz) Minute where
  UnixDateTimeNanos (base, ns) `datePlus` Minute m = UnixDateTimeNanos (base + fromIntegral (m * 60), ns)

instance DateTimeMath (UnixDateTimeNanos tz) Hour where
  UnixDateTimeNanos (base, ns) `datePlus` Hour hr = UnixDateTimeNanos (base + fromIntegral (hr * 60 * 60), ns)

javaDateTimeToUTCDateTime :: Int64 -> UnixDateTime 'UTC 'ISO8601
javaDateTimeToUTCDateTime = UnixDateTime . (`quot` 1000)

today :: IO (UnixDateTime 'UTC 'ISO8601)
today = do
  tm <- getCurrentDateTime
  return $! tm & hour .~ 0 & minute .~ 0 & second .~ 0

today' :: IO (UnixDate 'UTC 'ISO8601)
today' = do
  tm <- getCurrentDateTime
  return $! createDate' (tm ^. year) (tm ^. month) (tm ^. day)

isLeapYear :: Year 'ISO8601 -> Bool
isLeapYear (Year yr) = (yr `mod` 4 == 0) && (not (yr `mod` 100 == 0) || yr `mod` 400 == 0)

class HasDaysInMonth cal where
  daysInMonth :: Integral n => Year cal -> MonthOfYear cal -> n

instance HasDaysInMonth 'ISO8601 where
  daysInMonth _ January = 31
  daysInMonth yr February | isLeapYear yr = 29
                          | otherwise = 28
  daysInMonth _ March = 31
  daysInMonth _ April = 30
  daysInMonth _ May = 31
  daysInMonth _ June = 30
  daysInMonth _ July = 31
  daysInMonth _ August = 31
  daysInMonth _ September = 30
  daysInMonth _ October = 31
  daysInMonth _ November = 30
  daysInMonth _ December = 31

createTime :: Hour cal -> Minute cal -> Second cal -> UnixTime tz cal
createTime h m s = UnixTime 0 & hour .~ h & minute .~ m & second .~ s

createUTCDateTime :: Year cal -> Month cal -> Day cal -> Hour cal -> Minute cal -> Second cal -> UnixDateTime 'UTC cal
createUTCDateTime y mon d h m s = createDateTime y mon d h m s 0

createDateTime :: Year cal -> Month cal -> Day cal -> Hour cal -> Minute cal -> Second cal -> Int -> UnixDateTime tz cal
{-# INLINE createDateTime #-}
createDateTime (Year y) mon (Day d) (Hour h) (Minute m) (Second s) off =
  if y >= 1970 then UnixDateTime $ fromIntegral $ fromEnum ctime -- Note: There is an edgecase where the timezone off will push the date before 1970
  else error "createDateTime: Years before 1970 are not supported"
  where ctime :: CTime -- Apply the offset directly to the Hours/Minutes/Seconds because timegm ignores the offset member
        ctime = {-# SCC "ctime" #-}(convert $ C'tm (cint $ s - fromIntegral soff) (cint $ m - moff) (cint $ h - hoff) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = {-# SCC "cmon" #-}cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: (Integral a) => a -> CInt
        cint = {-# SCC "cint" #-}fromIntegral
        off32 = fromIntegral off
        (hoff, mofftot) = {-# SCC "hoff" #-}off32 `divMod` 3600
        (moff, soff) = {-# SCC "moff/soff" #-}mofftot `divMod` 60

createLocalDateTime :: Year cal -> Month cal -> Day cal -> Hour cal -> Minute cal -> Second cal -> IO (UnixDateTime 'UTC cal)
createLocalDateTime (Year y) mon (Day d) (Hour h) (Minute m) (Second s) =
  if y >= 1970 then do
    ctime <- convertLocal tm
    return $ UnixDateTime $ fromIntegral $ fromEnum ctime
  -- Note: There is an edgecase where the local timezone offset will push the date before 1970
  else error "createLocalDateTime: Years before 1970 are not supported"
  where tm = C'tm (cint s) (cint m) (cint h) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr
        cmon = cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: Integral a => a -> CInt
        cint = fromIntegral
        convertLocal tm' = with tm' (\tm_ptr -> c'mktime tm_ptr)

createUTCDateTimeNanos :: Year cal -> Month cal -> Day cal -> Hour cal -> Minute cal -> Second cal -> Millis cal -> UnixDateTimeNanos 'UTC cal
createUTCDateTimeNanos y mon d h m s ms = createDateTimeNanos y mon d h m s ms 0

createDateTimeNanos :: Year cal -> Month cal -> Day cal -> Hour cal -> Minute cal -> Second cal -> Millis cal -> Int -> UnixDateTimeNanos tz cal
createDateTimeNanos (Year y) mon (Day d) (Hour h) (Minute m) (Second s) (Millis ms) off =
  if y >= 1970 then UnixDateTimeNanos (fromIntegral $ fromEnum ctime, fromIntegral $ ms * 1000000)
  else error "createDateTimeNanos: Years before 1970 are not supported"
  where ctime :: CTime -- Apply the offset directly to the Hours/Minutes/Seconds because timegm ignores the offset member
        ctime = (convert $ C'tm (cint $ s - fromIntegral soff) (cint $ m - moff) (cint $ h - hoff) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: (Integral a) => a -> CInt
        cint = fromIntegral
        off32 = fromIntegral off
        (hoff, mofftot) = off32 `divMod` 3600
        (moff, soff) = mofftot `divMod` 60

createDate :: Year cal -> Month cal -> Day cal -> UnixDateTime tz cal
{-# INLINE createDate #-}
createDate (Year y) mon (Day d) =
  if y >= 1970 then UnixDateTime $ fromIntegral $ fromEnum ctime
  else error "createDateTime: Years before 1970 are not supported"
  where ctime :: CTime
        ctime = {-# SCC "ctime" #-}(convert $ C'tm 0 0 0 (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = {-# SCC "cmon" #-}cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: (Integral a) => a -> CInt
        cint = {-# SCC "cint" #-}fromIntegral

createDate' :: Year cal -> Month cal -> Day cal -> UnixDate tz cal
{-# INLINE createDate' #-}
createDate' (Year y) mon (Day d) =
  if y >= 1970 then UnixDate $ fromIntegral $ fromEnum ctime
  else error "createDateTime: Years before 1970 are not supported"
  where ctime :: CTime
        ctime = {-# SCC "ctime" #-}(convert $ C'tm 0 0 0 (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = {-# SCC "cmon" #-}cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: (Integral a) => a -> CInt
        cint = {-# SCC "cint" #-}fromIntegral

asDateTime :: UnixDate tz cal -> UnixDateTime tz cal
asDateTime (UnixDate dt) = UnixDateTime dt

haskellLocalTime :: Prism' (UnixDateTime 'UTC 'ISO8601) HT.LocalTime
haskellLocalTime = prism' g s where
  s dt = do
    let UTCDateTimeStruct{..} = view dateTimeStruct dt
        gtime = HT.TimeOfDay (fromIntegral $ dt_hour) (fromIntegral $ dt_minute) (fromIntegral $ dt_second)
    gday <- HT.fromGregorianValid (fromIntegral $ dt_year) (fromEnum $ dt_month) (fromIntegral $ dt_day)
    return $! HT.LocalTime gday gtime

  g (HT.LocalTime htday (HT.TimeOfDay h m s)) =
    let (y, mon, d) = {-# SCC "fromhlt_let" #-} HT.toGregorian htday
    in createUTCDateTime
         (Year $ fromIntegral y)
         (Month $ fromIntegral mon)
         (Day $ fromIntegral d)
         (Hour $ fromIntegral h)
         (Minute $ fromIntegral m)
         (Second $ truncate s)

haskellLocalTime' :: Prism' (UnixDateTimeNanos 'UTC 'ISO8601) HT.LocalTime
haskellLocalTime' = prism' g s where
  s dt@(UnixDateTimeNanos (_, ns)) = do
    let UTCDateTimeStruct{..} = view dateTimeStruct dt
        tod = HT.TimeOfDay (fromIntegral dt_hour) (fromIntegral dt_minute) (fromIntegral dt_second + fromIntegral ns / 1000000000)
    gday <- HT.fromGregorianValid (fromIntegral dt_year) (fromEnum dt_month) (fromIntegral dt_day)
    return $! HT.LocalTime gday tod

  g (HT.LocalTime htday (HT.TimeOfDay h m s)) = UnixDateTimeNanos (dt, truncate $ fpart * 1000000000) where
    (y, mon, d) = {-# SCC "fromhlt_let" #-} HT.toGregorian htday
    (sec, fpart) = properFraction s
    UnixDateTime dt = createUTCDateTime
      (Year $ fromIntegral y)
      (Month $ fromIntegral mon)
      (Day $ fromIntegral d)
      (Hour $ fromIntegral h)
      (Minute $ fromIntegral m)
      (Second $ sec)

haskellUTCTime :: Prism' (UnixDateTime 'UTC 'ISO8601) HT.UTCTime
haskellUTCTime = prism' g s where
  s dt = do
    let UTCDateTimeStruct{..} = view dateTimeStruct dt
        diffTime = HT.secondsToDiffTime $ fromIntegral dt_hour * 3600 + fromIntegral dt_minute * 60 + fromIntegral dt_second
    gday <- HT.fromGregorianValid (fromIntegral dt_year) (fromEnum dt_month) (fromIntegral dt_day)
    return $! HT.UTCTime gday diffTime

  g HT.UTCTime{utctDay = dt, utctDayTime = tm} =
    let (y, mon, d) = {-# SCC "toGregorian" #-} HT.toGregorian dt
        -- ts = round $ toRational tm
        ts = round tm
        (h, remsec) = ts `divMod` 3600
        (m, s) = remsec `divMod` 60
    in createUTCDateTime (Year $ fromIntegral y) (toEnum mon) (Day $ fromIntegral d) (Hour h) (Minute m) (Second (fromIntegral s))

haskellUTCTime' :: Prism' (UnixDateTimeNanos 'UTC 'ISO8601) HT.UTCTime
haskellUTCTime' = prism' g s where
  s dt@(UnixDateTimeNanos (_, ns)) = do
    let UTCDateTimeStruct{..} = view dateTimeStruct dt
        diffTime = HT.picosecondsToDiffTime ((fromIntegral dt_hour * 3600 + fromIntegral dt_minute * 60 + fromIntegral dt_second) * 1000000000000 + fromIntegral ns * 1000)
    gday <- HT.fromGregorianValid (fromIntegral dt_year) (fromEnum dt_month) (fromIntegral dt_day)
    return $! HT.UTCTime gday diffTime

  g HT.UTCTime{utctDay = dt, utctDayTime = tm} = UnixDateTimeNanos (dtm, truncate $ fpart * 1000000000) where
    (y, mon, d) = {-# SCC "toGregorian" #-}  HT.toGregorian dt
    (ts, fpart) = properFraction $ toRational tm
    (h, remsec) = ts `divMod` 3600
    (m, s) = remsec `divMod` 60
    UnixDateTime dtm = {-# SCC "fromHaskellUTCTime" #-} createUTCDateTime (Year $ fromIntegral y) (toEnum mon) (Day $ fromIntegral d) (Hour h) (Minute m) (Second (fromIntegral s))

haskellDate :: Iso' (UnixDate 'UTC 'ISO8601) HT.Day
haskellDate = iso g s where
  g dt
    | Year y  <- dt ^. year
    , Month m <- dt ^. month
    , Day d   <- dt ^. day
    = HT.fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)

  s dt = createDate' (fromIntegral y) (fromIntegral m) (fromIntegral d) where
    (y, m, d) = HT.toGregorian dt

fromTimezone :: FilePath -> UnixDateTime 'UTC 'ISO8601 -> UnixDateTime 'UTC 'ISO8601
fromTimezone tzPath dt =
  let tz = {-# SCC "loading_olson" #-}  unsafePerformIO $ getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ tzPath)
  in {-# SCC "fromTimezoneToUTC_in" #-} view (re haskellUTCTime) $ localTimeToUTC' tz . fromJust $ dt ^? haskellLocalTime

toTimezone :: FilePath -> UnixDateTime 'UTC 'ISO8601 -> UnixDateTime 'UTC 'ISO8601
toTimezone tzPath dt =
  let tz = {-# SCC "loading_olson" #-} unsafePerformIO $ getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ tzPath)
  in view (re haskellLocalTime) $ utcToLocalTime' tz . fromJust $ dt ^? haskellUTCTime

loadTimeZone :: String -> IO TimeZoneSeries
loadTimeZone tzPath = do
  getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ tzPath)

fromTimezoneToUTC' :: TimeZoneSeries -> Fold (UnixDateTime 'UTC 'ISO8601) (UnixDateTime 'UTC 'ISO8601)
fromTimezoneToUTC' tz = haskellLocalTime . to (localTimeToUTC' tz) . re haskellUTCTime

fromUTCToTimezone' :: TimeZoneSeries -> Fold (UnixDateTime 'UTC 'ISO8601) (UnixDateTime 'UTC 'ISO8601)
fromUTCToTimezone' tz = haskellUTCTime . to (utcToLocalTime' tz) . re haskellLocalTime

addDateTime :: UnixDate tz cal -> UnixTime tz cal -> UnixDateTime tz cal
addDateTime (UnixDate d) (UnixTime t) = UnixDateTime (d + t)

floorToInterval :: Int -> UnixDateTimeNanos tz cal -> UnixDateTimeNanos tz cal
floorToInterval maxInterval (UnixDateTimeNanos (s, ns)) = UnixDateTimeNanos (s, ns `div` maxIntNs * maxIntNs) where
  maxIntNs = fromIntegral maxInterval * 1000000

deltaInMillis :: UnixDateTimeNanos tz cal -> UnixDateTimeNanos tz cal -> Millis cal
deltaInMillis (UnixDateTimeNanos (s1, ns1)) (UnixDateTimeNanos (s2, ns2)) = fromIntegral ((s1 - s2) * 1000) + fromIntegral ((ns1 - ns2) `div` 1000000)

extract :: (C'tm -> CInt) -> Int64 -> Int32
extract fn val = unsafePerformIO $ do
  let ctime = fromIntegral val
  y <- with ctime (\ ctime' -> alloca (\ tm -> do
    tm' <- c'gmtime_r ctime' tm
    tm'' <- peek tm'
    return $ fn tm''))
  return $! convert y

cT2T :: CTime -> UnixTime tz cal
cT2T = UnixTime . fromIntegral . fromEnum

cT2D :: CTime -> UnixDate tz cal
cT2D = UnixDate . fromIntegral . fromEnum

cT2DT :: CTime -> UnixDateTime tz cal
cT2DT = UnixDateTime . fromIntegral . fromEnum

cT2DTm :: CTime -> Int32 -> UnixDateTimeNanos tz cal
cT2DTm ct ns = UnixDateTimeNanos (fromIntegral $ fromEnum ct, ns)

unixDateTimeToUnixDateIso :: Iso' (UnixDateTime tz cal) (UnixDate tz cal)
unixDateTimeToUnixDateIso = iso g s where
  g (UnixDateTime x) = UnixDate x
  s (UnixDate x)     = UnixDateTime x

unixDateTimeNanosToUnixDateIso :: Iso' (UnixDateTimeNanos tz cal) (UnixDate tz cal)
unixDateTimeNanosToUnixDateIso = iso g s where
  g (UnixDateTimeNanos (x, _)) = UnixDate x
  s (UnixDate x)               = UnixDateTimeNanos (x, 0)

class HasEpoch a cal where
  epoch :: Lens' (a cal) (Second cal)

instance HasEpoch (UnixDate tz) cal where
  epoch = iso g s where
    g (UnixDate s) = Second s
    s (Second s)   = UnixDate s


class HasEra a where
  era :: Enum (Era cal) => Lens' (a cal) (Era cal)

instance HasEra (UnixDate tz) where
  era = lens g s where
    g   _ = toEnum 0
    s x _ = error "era"

instance HasEra (UnixDateTime tz) where
  era = unixDateTimeToUnixDateIso . era

instance HasEra (UnixDateTimeNanos tz) where
  era = unixDateTimeNanosToUnixDateIso . era


class HasCentury a where
  century :: Lens' (a cal) (Century cal)

instance HasCentury (UnixDate tz) where
  century = iplens g s where
    g   _ = Century 0
    s x _ = x

instance HasCentury (UnixDateTime tz) where
  century = unixDateTimeToUnixDateIso . century where

instance HasCentury (UnixDateTimeNanos tz) where
  century = unixDateTimeNanosToUnixDateIso . century where


class HasYearOfCentury a where
  yearOfCentury :: Lens' (a cal) (Year cal)

instance HasYearOfCentury (UnixDate tz) where
  yearOfCentury = lens g s where
    g (UnixDate x)  = Year $ extract c'tm'tm_year x `mod` 100
    s _ _ = error "yearOfCentury"

instance HasYearOfCentury (UnixDateTime tz) where
  yearOfCentury = unixDateTimeToUnixDateIso . yearOfCentury

instance HasYearOfCentury (UnixDateTimeNanos tz) where
  yearOfCentury = unixDateTimeNanosToUnixDateIso . yearOfCentury


class HasYearOfEra a where
  yearOfEra :: Lens' (a cal) (Year cal)

instance HasYearOfEra (UnixDate tz) where
  yearOfEra = lens g s where
    g (UnixDate x) = Year $ 1900 + extract c'tm'tm_year x
    s _ _ = error "yearOfEra"

instance HasYearOfEra (UnixDateTime tz) where
  yearOfEra = unixDateTimeToUnixDateIso . yearOfEra

instance HasYearOfEra (UnixDateTimeNanos tz) where
  yearOfEra = unixDateTimeNanosToUnixDateIso . yearOfEra


class HasDayOfYear a where
  dayOfYear :: Lens' (a cal) (Day cal)

instance HasDayOfYear (UnixDate tz) where
  dayOfYear = lens g s where
    g (UnixDate x) = Day $ extract c'tm'tm_yday x
    s _ _ = error "dayOfYear"

instance HasDayOfYear (UnixDateTime tz) where
  dayOfYear = unixDateTimeToUnixDateIso . dayOfYear

instance HasDayOfYear (UnixDateTimeNanos tz) where
  dayOfYear = unixDateTimeNanosToUnixDateIso . dayOfYear


class HasYear a where
  year :: Lens' (a cal) (Year cal)

instance HasYear (UnixDate tz) where
  year = lens g s where
    g (UnixDate x) = Year $ 1900 + extract c'tm'tm_year x
    s (UnixDate x) (Year y) = cT2D $ withField (fromIntegral x) (\x -> x { c'tm'tm_year = fromIntegral (y - 1900) })

instance HasYear (UnixDateTime tz) where
  year = unixDateTimeToUnixDateIso . year

instance HasYear (UnixDateTimeNanos tz) where
  year = unixDateTimeNanosToUnixDateIso . year


class HasMonth a where
  month :: Lens' (a cal) (Month cal)

instance HasMonth (UnixDate tz) where
  month = lens g s where
    g (UnixDate x) = fromIntegral $ 1 + extract c'tm'tm_mon x
    s (UnixDate x) (Month m) = cT2D $ withField (fromIntegral x) (\x -> x { c'tm'tm_mon = fromIntegral $ (fromEnum m - 1) })

instance HasMonth (UnixDateTime tz) where
  month = unixDateTimeToUnixDateIso . month

instance HasMonth (UnixDateTimeNanos tz) where
  month = unixDateTimeNanosToUnixDateIso . month


class HasDay a where
  day :: Lens' (a cal) (Day cal)

instance HasDay (UnixDate tz) where
  day = lens g s where
    g (UnixDate x) = Day $ extract c'tm'tm_mday x
    s (UnixDate x) (Day d) = cT2D $ withField (fromIntegral x) (\x -> x { c'tm'tm_mday = fromIntegral d })

instance HasDay (UnixDateTime tz) where
  day = unixDateTimeToUnixDateIso . day

instance HasDay (UnixDateTimeNanos tz) where
  day = unixDateTimeNanosToUnixDateIso . day


class HasDayOfWeek a where
  dayOfWeek :: Enum (DayOfWeek cal) => Getter (a cal) (DayOfWeek cal)

instance HasDayOfWeek (UnixDate tz) where
  dayOfWeek = to $ \ (UnixDate x) -> toEnum . fromIntegral $ extract c'tm'tm_wday x

instance HasDayOfWeek (UnixDateTime tz) where
  dayOfWeek = to $ \ (UnixDateTime x) -> toEnum . fromIntegral $ extract c'tm'tm_wday x

instance HasDayOfWeek (UnixDateTimeNanos tz) where
  dayOfWeek = to $ \ (UnixDateTimeNanos (x, _)) -> toEnum . fromIntegral $ extract c'tm'tm_wday x


class HasDateStruct a where
  dateStruct :: Iso' (a cal) (UTCDateStruct cal)

instance HasDateStruct (UnixDate tz) where
  dateStruct = iso toDateStruct fromDateStruct where
    toDateStruct (UnixDate dt) = UTCDateStruct y m d where
      tm = convert $ (fromIntegral dt :: CTime)
      y = Year  . fromIntegral $ c'tm'tm_year tm + 1900
      m = Month . fromIntegral $ c'tm'tm_mon  tm + 1
      d = Day   . fromIntegral $ c'tm'tm_mday tm
    fromDateStruct UTCDateStruct{d_day=d, d_month=m, d_year=y} = createDate' y m d

instance HasDateStruct (UnixDateTime tz) where
  dateStruct = unixDateTimeToUnixDateIso . dateStruct

instance HasDateStruct (UnixDateTimeNanos tz) where
  dateStruct = unixDateTimeNanosToUnixDateIso . dateStruct


class HasDateTimeStruct a where
  dateTimeStruct :: Iso' (a cal) (UTCDateTimeStruct cal)

instance HasDateTimeStruct (UnixDateTime tz) where
  dateTimeStruct = iso toStruct fromStruct where
    toStruct (UnixDateTime dt) = UTCDateTimeStruct (Year $ (fromIntegral $ c'tm'tm_year tm) + 1900)
                                                   (toEnum $ (fromIntegral $ c'tm'tm_mon tm) + 1)
                                                   (Day $ fromIntegral $ c'tm'tm_mday tm)
                                                   (Hour $ fromIntegral $ c'tm'tm_hour tm)
                                                   (Minute $ fromIntegral $ c'tm'tm_min tm)
                                                   (Second $ fromIntegral $ c'tm'tm_sec tm)
                                                   0
      where tm = (convert $ ((fromIntegral dt) :: CTime)) :: C'tm

    fromStruct UTCDateTimeStruct { dt_second=s, dt_minute=m, dt_hour=h, dt_day=d, dt_month=mon, dt_year=y } =
      createDateTime y mon d h m s 0


instance HasDateTimeStruct (UnixDateTimeNanos tz) where
  dateTimeStruct = iso toStruct fromStruct where
    toStruct (UnixDateTimeNanos (dt, ns)) = UTCDateTimeStruct (Year $ (fromIntegral $ c'tm'tm_year tm) + 1900)
                                                              (toEnum $ (fromIntegral $ c'tm'tm_mon tm) + 1)
                                                              (Day $ fromIntegral $ c'tm'tm_mday tm)
                                                              (Hour $ fromIntegral $ c'tm'tm_hour tm)
                                                              (Minute $ fromIntegral $ c'tm'tm_min tm)
                                                              (Second $ fromIntegral $ c'tm'tm_sec tm)
                                                              (Millis $ fromIntegral $ ns `div` 1000000)
      where tm = (convert $ ((fromIntegral dt) :: CTime)) :: C'tm

    fromStruct UTCDateTimeStruct { dt_millis=ms, dt_second=s, dt_minute=m, dt_hour=h, dt_day=d, dt_month=mon, dt_year=y } =
      createDateTimeNanos y mon d h m s ms 0


class HasHour a where
  hour :: Lens' (a cal) (Hour cal)

instance HasHour (UnixTime tz) where
  hour = lens g s where
    g (UnixTime x) = Hour $ extract c'tm'tm_hour x
    s (UnixTime x) (Hour h) = cT2T $ withField (fromIntegral x) (\x -> x { c'tm'tm_hour = fromIntegral h })

instance HasHour (UnixDateTime tz) where
  hour = lens g s where
    g (UnixDateTime x) = Hour $ extract c'tm'tm_hour x
    s (UnixDateTime x) (Hour h) = cT2DT $ withField (fromIntegral x) (\x -> x { c'tm'tm_hour = fromIntegral h })

instance HasHour (UnixDateTimeNanos tz) where
  hour = lens g s where
    g (UnixDateTimeNanos (x, _)) = Hour $ extract c'tm'tm_hour x
    s (UnixDateTimeNanos (x, ns)) (Hour h) = cT2DTm (withField (fromIntegral x) (\x -> x { c'tm'tm_hour = fromIntegral h })) ns


class HasMinute a where
  minute :: Lens' (a cal) (Minute cal)

instance HasMinute (UnixTime tz) where
  minute = lens g s where
    g (UnixTime x) = Minute $ extract c'tm'tm_min x
    s (UnixTime x) (Minute m) = cT2T $ withField (fromIntegral x) (\x -> x { c'tm'tm_min = fromIntegral m })

instance HasMinute (UnixDateTime tz) where
  minute = lens g s where
    g (UnixDateTime x) = Minute $ extract c'tm'tm_min x
    s (UnixDateTime x) (Minute m) = cT2DT $ withField (fromIntegral x) (\x -> x { c'tm'tm_min = fromIntegral m })

instance HasMinute (UnixDateTimeNanos tz) where
  minute = lens g s where
    g (UnixDateTimeNanos (x, _)) = UnixDateTime x ^. minute
    s (UnixDateTimeNanos (x, ns)) (Minute m) = cT2DTm (withField (fromIntegral x) (\x -> x { c'tm'tm_min = fromIntegral m })) ns


class HasSecond a where
  second :: Lens' (a cal) (Second cal)

instance HasSecond (UnixTime tz) where
  second = lens g s where
    g (UnixTime x) = Second . fromIntegral $ extract c'tm'tm_sec x
    s (UnixTime x) (Second s) = cT2T $ withField (fromIntegral x) (\x -> x { c'tm'tm_sec = fromIntegral s })

instance HasSecond (UnixDateTime tz) where
  second = lens g s where
    g (UnixDateTime x) = Second . fromIntegral $ extract c'tm'tm_sec x
    s (UnixDateTime x) (Second s) = cT2DT $ withField (fromIntegral x) (\x -> x { c'tm'tm_sec = fromIntegral s })

instance HasSecond (UnixDateTimeNanos tz) where
  second = lens g s where
    g (UnixDateTimeNanos (x, _)) = UnixDateTime x ^. second
    s (UnixDateTimeNanos (x, ns)) (Second s) = cT2DTm (withField (fromIntegral x) (\x -> x { c'tm'tm_sec = fromIntegral s })) ns


class HasMillisOfDay a where
  millisOfDay :: Getter (a cal) (Millis cal)

instance HasMillisOfDay (UnixDateTimeNanos tz) where
  millisOfDay = to $ \ (UnixDateTimeNanos (x, ns)) ->
    Millis $ fromIntegral (getSeconds (UnixDateTime x ^. secondOfDay)) * 1000 + (fromIntegral ns `div` 1000000)


class HasMillis a where
  millis :: Lens' (a cal) (Millis cal)

instance HasMillis (UnixDateTimeNanos tz) where
  millis = lens g s where
    g (UnixDateTimeNanos (_, x)) = Millis $ fromIntegral x `div` 1000000 -- Convert Nanoseconds to Milliseconds
    s (UnixDateTimeNanos (x, _)) (Millis ms)
      | s == 0    = UnixDateTimeNanos (x, ms' * 1000000)
      | otherwise = UnixDateTimeNanos (x, ms' * 1000000) `datePlus` Second (fromIntegral s)
      where (s, ms') = (uncurry ((,) `on` fromIntegral)) $ ms `divMod` 1000


class HasNanos a where
  nanos :: Lens' (a cal) (Nanos cal)

instance HasNanos (UnixDateTimeNanos tz) where
  nanos = lens g s where
    g (UnixDateTimeNanos (_,x)) = Nanos $ fromIntegral x `mod` 1000000000
    s (UnixDateTimeNanos (dt, _)) (Nanos ns)
      | s == 0    = UnixDateTimeNanos (dt, ns')
      | otherwise = UnixDateTimeNanos (dt, ns') `datePlus` Second (fromIntegral s)
      where (s, ns') = (uncurry ((,) `on` fromIntegral)) $ ns `divMod` 1000000000


class HasMinuteOfDay a where
  minuteOfDay :: Getter (a cal) (Minute cal)

instance HasMinuteOfDay (UnixTime tz) where
  minuteOfDay = to $ \ (UnixTime x) -> Minute $ extract c'tm'tm_min x + (extract c'tm'tm_hour x * 60)

instance HasMinuteOfDay (UnixDateTime tz) where
  minuteOfDay = to $ \ (UnixDateTime x) -> Minute $ extract c'tm'tm_min x + (extract c'tm'tm_hour x * 60)

instance HasMinuteOfDay (UnixDateTimeNanos tz) where
  minuteOfDay = to $ \ (UnixDateTimeNanos (x, _)) -> Minute $ extract c'tm'tm_min x + (extract c'tm'tm_hour x * 60)


class HasSecondOfDay a where
  secondOfDay :: Getter (a cal) (Second cal)

instance HasSecondOfDay (UnixTime tz) where
  secondOfDay = to (\ (UnixTime tm) -> UnixDateTime tm) . secondOfDay

instance HasSecondOfDay (UnixDateTime tz) where
  secondOfDay = to (\ (UnixDateTime tm) -> UnixDateTimeNanos (tm, 0)) . secondOfDay

instance HasSecondOfDay (UnixDateTimeNanos tz) where
  secondOfDay = to $ \ tm ->
    let UTCDateTimeStruct{dt_second = Second s, dt_minute = Minute m, dt_hour = Hour h} = view dateTimeStruct tm
    in Second $ s + fromIntegral m * 60 + fromIntegral h * 3600
