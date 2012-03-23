{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AlphaHeavy.Time
  ( DateTime(..)
  , DateTimeLens
  , TimeZone(..)
  , TimeZoneOffset(..)

  -- * Raw date components
  , DateTimeComponents(..)
  , DateTimeStruct(..)

  -- * System time
  , getCurrentDateTimeNanos

  -- * Lens accessors
  , AlphaHeavy.Time.get
  , AlphaHeavy.Time.set

  -- * Date storage
  , UnixTime(..)
  , UnixTimeNanos(..)
  , JavaTime
  , JulianDate

  -- * Date sections
  , Year(..)
  , Month(..)
  , Week(..)
  , Day(..)
  , Hour(..)
  , Minute(..)
  , Second(..)
  , Milli(..)
  , Nano(..)
  , Pico(..)

  -- * Helper lenses
  , year
  , month
  , monthOfYear
  , week
  , day
  , hour
  , minute
  , second
  , milli
  , nano
  , pico
  ) where

{-
module AlphaHeavy.Time (
    Day(..)
  , Hours(..)
  , Minutes(..)
  , Seconds(..)
  , Millis(..)
  , Month(..)
  , Week(..)
  , Year(..)
  , Century(..)
  , Era(..)
  , Nanos(..)
  , DayOfWeek(..)

  , Chronology
  , TimeZone(..)
  , Date(..)
  , Time(..)
  , DateTime(..)
  , DateTimeMath(..)

  , UTCDate(..)
  , UTCTime(..)
  , UTCDateTime(..)
  , UTCDateTimeNanos(..)

  , UTCDateTimeStruct(..)

  , addDateTime
  , addMillis
  , deltaInMillis
  , floorToInterval

  , createDateTime
  , createUTCDateTime
  , createLocalDateTime
  , createDateTimeNanos
  , createUTCDateTimeNanos

  , fromHaskellLocalTime
  , fromHaskellUTCTime
  , toHaskellLocalTime
  , toHaskellUTCTime

  , fromTimezone
  , fromTimezoneToUTC'
  , toTimezone
  , fromUTCToTimezone'
  , loadTimeZone

  , getCurrentDateTime
  , getCurrentDateTimeNanos
  , javaDateTimeToUTCDateTime
  , dateTimeAsString
  , today

  , calculateDuration
  , Duration(..)
) where
-}

import AlphaHeavy.Time.TM
import Control.Arrow (Kleisli(..), runKleisli, arr)
import Control.Category
import Control.DeepSeq
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy as State
import Data.Convertible
import Data.Int
import Data.Label as Dl
import Data.Label.Abstract as A
-- import Data.Label.Maybe as DlM
import Data.Data
import Data.Maybe (fromJust)
import qualified Data.Time as HT
import Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile)
import Data.Time.LocalTime.TimeZone.Series (localTimeToUTC',TimeZoneSeries, utcToLocalTime')
import Foreign (Ptr,nullPtr,alloca,with,peek)
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import Prelude hiding ((.), id)

data DateTimeStruct tz = DateTimeStruct
  { dt_year   :: Year tz
  , dt_month  :: Month tz
  , dt_day    :: Day tz
  , dt_hour   :: Hour tz
  , dt_minute :: Minute tz
  , dt_second :: Second tz
  , dt_nanos  :: Nano tz
  } -- deriving -- (Eq,Show)

newtype Era     tz = Era     {getEra     :: Int32} -- deriving (Eq,Ord,Num,Real,Enum,Integral,Show,Read,Data,Typeable,NFData)
newtype Century tz = Century {getCentury :: Int32} -- deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,Data,Typeable,NFData)
newtype Week    tz = Weeks   {getWeek    :: Int32} -- deriving (Eq,Ord,Num,Real,Enum,Integral,Show,Read,Data,Typeable,NFData)

newtype TimeZoneOffset tz = TimeZoneOffset {getTimeZoneOffset :: Int32}
deriving instance Show   (TimeZoneOffset tz)
deriving instance NFData (TimeZoneOffset tz)

newtype Year tz = Years {getYear :: Int32} -- deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,Data,Typeable,NFData)
deriving instance Read   (Year tz)
deriving instance Show   (Year tz)
deriving instance Num    (Year tz)
deriving instance NFData (Year tz)

data Month tz
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

deriving instance Eq     (Month tz)
deriving instance Ord    (Month tz)
deriving instance Read   (Month tz)
deriving instance Show   (Month tz)

instance NFData (Month tz) where
  rnf x = x `seq` ()

instance Enum (Month tz) where
  toEnum 1 = January
  toEnum 2 = February
  toEnum 3 = March
  toEnum 4 = April
  toEnum 5 = May
  toEnum 6 = June
  toEnum 7 = July
  toEnum 8 = August
  toEnum 9 = September
  toEnum 10 = October
  toEnum 11 = November
  toEnum 12 = December
  toEnum _ = error "Bad Argument"
  fromEnum January = 1
  fromEnum February = 2
  fromEnum March = 3
  fromEnum April = 4
  fromEnum May = 5
  fromEnum June = 6
  fromEnum July = 7
  fromEnum August = 8
  fromEnum September = 9
  fromEnum October = 10
  fromEnum November = 11
  fromEnum December = 12

instance Bounded (Month tz) where
  minBound = January
  maxBound = December

newtype Day tz = Day {getDay :: Int32} -- deriving (Eq,Ord,Num,Real,Enum,Integral,Show,Read,Data,Typeable,NFData)
deriving instance Read   (Day tz)
deriving instance Show   (Day tz)
deriving instance Num    (Day tz)
deriving instance NFData (Day tz)

data DayOfWeek
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
    deriving (Eq,Ord,Enum,Show,Read,Typeable)

instance Bounded DayOfWeek where
  minBound = Sunday
  maxBound = Saturday

newtype Hour tz = Hours {getHour :: Int32}
deriving instance Read   (Hour tz)
deriving instance Show   (Hour tz)
deriving instance Num    (Hour tz)
deriving instance NFData (Hour tz)

newtype Minute tz = Minutes {getMinute :: Int32}
deriving instance Read   (Minute tz)
deriving instance Show   (Minute tz)
deriving instance Num    (Minute tz)
deriving instance NFData (Minute tz)

newtype Second tz = Seconds {getSecond :: Int32}
deriving instance Read   (Second tz)
deriving instance Show   (Second tz)
deriving instance Num    (Second tz)
deriving instance NFData (Second tz)

newtype Milli tz = Millis {getMillis :: Int64}
deriving instance Read   (Milli tz)
deriving instance Show   (Milli tz)
deriving instance Num    (Milli tz)
deriving instance NFData (Milli tz)

newtype Nano tz = Nanos {getNanos :: Int64}
deriving instance Read   (Nano tz)
deriving instance Show   (Nano tz)
deriving instance Num    (Nano tz)
deriving instance NFData (Nano tz)

newtype Pico tz = Picos {getPicos :: Int64}
deriving instance Read   (Pico tz)
deriving instance Show   (Pico tz)
deriving instance Num    (Pico tz)
deriving instance NFData (Pico tz)

-- class Chronology c (t :: TimeZone)

-- class TimeZone a where
  -- offset :: a -> Second

-- instance TimeZone a where
  -- offset _ = Second 0

data TimeZone = UTC | LocalTime -- NamedTimeZone String

newtype UnixTime (t :: TimeZone)      = UnixTime Int64 deriving (Num,Ord,Eq,NFData)
deriving instance Show (UnixTime tz)

newtype UnixTimeNanos (t :: TimeZone) = UnixTimeNanos (Int64, Int32) deriving (Ord,Eq,NFData)
deriving instance Show (UnixTimeNanos tz)

newtype JavaTime (t :: TimeZone)      = JavaTime Int64

newtype JulianDate (t :: TimeZone)    = JulianDate Int64

-- instance Chronology UnixTime t
-- instance Chronology UnixTimeNanos t

-- type (f, tz) :~> a = A.Lens (Kleisli (State (DateTimeStruct tz))) f a

dateTimeLens
  :: (f tz -> State (DateTimeStruct tz) (a tz))
  -> (a tz -> f tz -> State (DateTimeStruct tz) (f tz))
  -- -> A.Lens (Kleisli (State (DateTimeStruct tz))) f a
  -> DateTimeLens f tz a
dateTimeLens g s = A.lens (Kleisli g) (Kleisli (uncurry s))

class DateTimeComponents f tz where
  unpack :: f tz -> DateTimeStruct tz
  pack   :: DateTimeStruct tz -> f tz

instance DateTimeComponents UnixTimeNanos tz where
  unpack = unpackUnixTimeNanos
  pack   = packUnixTimeNanos

instance DateTimeComponents UnixTime tz where
  unpack = unpackUnixTime
  pack   = packUnixTime

runDateTimeLens
  :: forall f a tz . DateTimeComponents f tz
  => Kleisli (State (DateTimeStruct tz)) (f tz) a
  -> f tz
  -> a
runDateTimeLens l f = evalState (runKleisli l f) (unpack f)

get
  :: DateTimeComponents f tz
  => DateTimeLens f tz a
  -> f tz
  -> a tz
get = runDateTimeLens . A.get

set
  :: DateTimeComponents f tz
  => DateTimeLens f tz a
  -> a tz
  -> f tz
  -> f tz
set l v = runDateTimeLens (A.set l . arr (v,))

-- TODO: allow timezone conversions
convertDateTime
  :: (DateTimeComponents f tz, DateTimeComponents f' tz)
  => f  tz
  -> f' tz
convertDateTime = pack . unpack

unpackUnixTimeNanos :: UnixTimeNanos tz -> DateTimeStruct tz
unpackUnixTimeNanos (UnixTimeNanos (s, ns)) = val' where
  val'   = val{dt_nanos = nano}
  val    = unpackUnixTime (UnixTime s)
  nano   = Nanos   . fromIntegral $ ns

packUnixTimeNanos :: DateTimeStruct tz -> UnixTimeNanos tz
packUnixTimeNanos dts@DateTimeStruct{..} = val' where
  val'       = UnixTimeNanos (s, fromIntegral ns)
  UnixTime s = packUnixTime dts
  Nanos ns   = dt_nanos

unpackUnixTime :: UnixTime tz -> DateTimeStruct tz
unpackUnixTime (UnixTime s) = val where
  val    = DateTimeStruct year month day hour minute second 0
  year   = Years   . fromIntegral $ c'tm'tm_year tm + 1900
  month  = toEnum  . fromIntegral $ c'tm'tm_mon  tm + 1
  day    = Day     . fromIntegral $ c'tm'tm_mday tm
  hour   = Hours   . fromIntegral $ c'tm'tm_hour tm
  minute = Minutes . fromIntegral $ c'tm'tm_min  tm
  second = Seconds . fromIntegral $ c'tm'tm_sec  tm
  tm     = convert (fromIntegral s :: CTime)

packUnixTime :: DateTimeStruct tz -> UnixTime tz
packUnixTime DateTimeStruct{..} = UnixTime (fromIntegral val') where
  val' :: Int
  val' = convert val
  val :: CTime
  val = convert C'tm
    { c'tm'tm_year   = fromIntegral $ getYear   dt_year  - 1900
    , c'tm'tm_mon    = fromIntegral $ fromEnum  dt_month - 1
    , c'tm'tm_mday   = fromIntegral $ getDay    dt_day
    , c'tm'tm_hour   = fromIntegral $ getHour   dt_hour
    , c'tm'tm_min    = fromIntegral $ getMinute dt_minute
    , c'tm'tm_sec    = fromIntegral $ getSecond dt_second
    , c'tm'tm_wday   = 0
    , c'tm'tm_yday   = 0
    , c'tm'tm_isdst  = 0
    , c'tm'tm_gmtoff = 0
    , c'tm'tm_zone   = nullPtr
    }

type DateTimeLens a (tz :: TimeZone) b =
  A.Lens (Kleisli (State (DateTimeStruct tz))) (a tz) (b tz)

class DateTime (a :: TimeZone -> *) (tz :: TimeZone) (b :: TimeZone -> *) where
  datePart :: DateTimeLens a tz b

instance DateTimeComponents base tz => DateTime base tz Year where
  datePart  = dateTimeLens g s where
    g _     = State.gets dt_year
    s val _ = do
      s <- State.get
      let s' = s{dt_year = val}
      State.put s'
      return (pack s')

instance DateTimeComponents base tz => DateTime base tz Month where
  datePart  = dateTimeLens g s where
    g _     = State.gets dt_month
    s val r = do
      s <- State.get
      let s' = s{dt_month = val}
      State.put s'
      return (pack s')

instance DateTimeComponents base tz => DateTime base tz Day where
  datePart  = dateTimeLens g s where
    g _     = State.gets dt_day
    s val r = do
      s <- State.get
      let s' = s{dt_day = val}
      State.put s'
      return (pack s')

instance DateTimeComponents base tz => DateTime base tz Hour where
  datePart  = dateTimeLens g s where
    g _     = State.gets dt_hour
    s val r = do
      s <- State.get
      let s' = s{dt_hour = val}
      State.put s'
      return (pack s')

instance DateTimeComponents base tz => DateTime base tz Minute where
  datePart  = dateTimeLens g s where
    g _     = State.gets dt_minute
    s val r = do
      s <- State.get
      let s' = s{dt_minute = val}
      State.put s'
      return (pack s')

instance DateTimeComponents base tz => DateTime base tz Second where
  datePart  = dateTimeLens g s where
    g _     = State.gets dt_second
    s val r = do
      s <- State.get
      let s' = s{dt_second = val}
      State.put s'
      return (pack s')

instance DateTimeComponents base tz => DateTime base tz Nano where
  datePart  = dateTimeLens g s where
    g _     = State.gets dt_nanos
    s val r = do
      s <- State.get
      let s' = s{dt_nanos = val}
      State.put s'
      return (pack s')

{-
instance forall base tz . DateTimeComponents base tz => DateTime base tz Milli where
  datePart  = dateTimeLens g s where
    -- g :: base tz -> State (DateTimeStruct tz) (Milli tz)
    g _     = do
      Nanos ns <- State.gets dt_nanos
      return $! Millis (ns `div` 1000000)
      -- fmap (\ (Nanos x) -> Millis (x `div` 1000000)) (State.gets dt_nanos)

    s (Millis val) r = do
      s <- State.get
      let s' = s{dt_nanos = Nanos (val * 1000000)}
      State.put s'
      return (pack s')
-}

instance DateTime Year tz Month where
  datePart = dateTimeLens g s where
    g _     = State.gets dt_month
    s val r = do
      State.modify (\ s -> s{dt_month = val})
      return r

{-
instance DateTime Year tz Day where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_day
   s val r = do
     State.modify (\ s -> s{dt_day = val})
     return r
-}

instance DateTime Month tz Day where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_day
   s val r = do
     State.modify (\ s -> s{dt_day = val})
     return r

instance DateTime Day tz Hour where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_hour
   s val r = do
     State.modify (\ s -> s{dt_hour = val})
     return r

instance DateTime Hour tz Minute where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_minute
   s val r = do
     State.modify (\ s -> s{dt_minute = val})
     return r

instance DateTime Minute tz Second where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_second
   s val r = do
     State.modify (\ s -> s{dt_second = val})
     return r

{-
instance DateTime Second tz Milli where
  datePart = dateTimeLens g s where
   g _     = do
     -- Millis (getNanos (State.gets dt_nanos) `div` 1000)
     ns <- State.gets dt_nanos
     -- let ns = Nanos 1000
     return (Millis (getNanos ns `div` 1000))

   s val r = do
     State.modify (\ s -> s{dt_nanos = fromIntegral (val * 1000)})
     return r
-}

{-
instance DateTime UnixTimeNanos tz Hour where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_hour
   s val r = do
     State.modify (\ s -> s{dt_hour = val})
     return r

instance DateTime UnixTimeNanos tz Minute where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_minute
   s val r = do
     State.modify (\ s -> s{dt_minute = val})
     return r

instance DateTime UnixTimeNanos tz Second where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_second
   s val r = do
     State.modify (\ s -> s{dt_second = val})
     return r

instance DateTime UnixTimeNanos tz Milli where
  datePart = dateTimeLens g s where
   g _     = State.gets dt_millis
   s val r = do
     State.modify (\ s -> s{dt_millis = val})
     return r
-}

year
  :: DateTime a tz Year
  => DateTimeLens a tz Year
year = datePart

month
  :: DateTime a tz Month
  => DateTimeLens a tz Month
month = datePart

monthOfYear
  :: DateTime a tz Year
  => DateTimeLens a tz Month
monthOfYear = month . year

week
  :: DateTime a tz Week
  => DateTimeLens a tz Week
week = datePart

day
  :: DateTime a tz Day
  => DateTimeLens a tz Day
day = datePart

hour
  :: DateTime a tz Hour
  => DateTimeLens a tz Hour
hour = datePart

minute
  :: DateTime a tz Minute
  => DateTimeLens a tz Minute
minute = datePart

second
  :: DateTime a tz Second
  => DateTimeLens a tz Second
second = datePart

secondOfDay
  :: (DateTime a tz Day, DateTime Day tz Second)
  => DateTimeLens a tz Second
secondOfDay = second . day

milli
  :: DateTime a tz Milli
  => DateTimeLens a tz Milli
milli = datePart

nano
  :: DateTime a tz Nano
  => DateTimeLens a tz Nano
nano = datePart

pico
  :: DateTime a tz Pico
  => DateTimeLens a tz Pico
pico = datePart

timeZoneOffset
  :: DateTime a tz TimeZoneOffset
  => DateTimeLens a tz TimeZoneOffset
timeZoneOffset = datePart

instance DateTime a UTC TimeZoneOffset where
  datePart = dateTimeLens (\ _ -> return (TimeZoneOffset 0)) (\ _ _ -> fail "asdfasdfasdf")

{-
instance Date UnixTimeNanos tz DateTimeStruct where
  datePart = DlM.lens (Just . unixTimeNanosToStruct) (\ _ _ -> Nothing)
-}



{-
class (Chronology a b) => Date a (b :: TimeZone) where
  year  :: a b :~> Year
  month :: a b :~> Month
  week  :: a b :~> Week
  day   :: a b :~> Day

class (Chronology a b) => Time a (b :: TimeZone) where
  hour           :: a b :~> Hours
  minuteOfDay    :: a b :~> Minutes
  minute         :: a b :~> Minutes
  secondOfDay    :: a b :~> Seconds
  second         :: a b :~> Seconds
  millisOfDay    :: a b :~> Millis
  millis         :: a b :~> Millis
  nanos          :: a b :~> Nanos
  picos          :: a b :~> Picos

class (Date a b, Time a b, Chronology a b) => DateTime a (b :: TimeZone) where
  type TimeRep a b :: *
  type DateRep a b :: *

  date            :: a b :-> DateRep a b
  time            :: a b :-> TimeRep a b

  toStruct        :: a b -> UTCDateTimeStruct
  fromStruct      :: UTCDateTimeStruct -> a b

  convertTimeZone :: DateTime a (c :: TimeZone) => a b -> a c
  convertTimeZone = fromStruct . toStruct

instance Date UnixTime 'UTCTimeZone where
  century = DlM.lens (\ _ -> Nothing) (\ _ _ -> Nothing)
  day     = DlM.lens (\ _ -> Nothing) (\ _ _ -> Nothing)

-- instance Date UnixTimeNanos 'UTCTimeZone where
instance Date UnixTimeNanos t where
  century = DlM.lens (\ _ -> Nothing) (\ _ _ -> Nothing)
  day     = DlM.lens (\ _ -> Nothing) (\ _ _ -> Nothing)
-}

getCurrentDateTimeNanos :: IO (UnixTimeNanos 'UTC)
getCurrentDateTimeNanos = do
  C'timeval{c'timeval'tv_sec = sec, c'timeval'tv_usec = ms} <- getTimeOfDay
  return $! UnixTimeNanos (fromIntegral sec, fromIntegral (ms * 1000))

{-
  century _                               = Century 0
  day (UTCDateTimeNanos (x,_))            = day (UTCDateTime x)
  dayOfWeek (UTCDateTimeNanos (x,_))      = dayOfWeek (UTCDateTime x)
  dayOfYear (UTCDateTimeNanos (x,_))      = dayOfYear (UTCDateTime x)
  era _                                   = Era 1 -- AD
  month (UTCDateTimeNanos (x,_))          = month (UTCDateTime x)
  year (UTCDateTimeNanos (x,_))           = year (UTCDateTime x)
  yearOfCentury (UTCDateTimeNanos (x,_))  = yearOfCentury (UTCDateTime x)
  yearOfEra (UTCDateTimeNanos (x,_))      = yearOfEra (UTCDateTime x)
  weekOfWeekyear (UTCDateTimeNanos (x,_)) = weekOfWeekyear (UTCDateTime x)
  weekyear (UTCDateTimeNanos (x,_))       = weekyear (UTCDateTime x)
-}

-- http://joda-time.sourceforge.net/field.html

{-
newtype UTCTime = UTCTime Int64 deriving (Show,Num,Ord,Eq,NFData)
newtype UTCDate = UTCDate Int64 deriving (Show,Num,Ord,Eq,NFData)
newtype UTCDateTime = UTCDateTime Int64 deriving (Num,Ord,Eq,NFData)
newtype UTCDateTimeNanos = UTCDateTimeNanos (Int64, Int32) deriving (Ord,Eq,NFData)
-}

{-
newtype Duration = Duration Int64 deriving (Show,Num,Eq,NFData)

-- calculateDuration :: UTCDateTime -> UTCDateTime -> Duration
calculateDuration :: DateTime a b => UTCDateTime -> UTCDateTime -> Duration
calculateDuration dt1 dt2 =
  let UTCDateTime dt1' = dt1
      UTCDateTime dt2' = dt2
  in  Duration (dt1' - dt2')

instance NFData UTCDateTimeStruct where
  rnf UTCDateTimeStruct{..} =
    rnf dt_millis `seq`
    rnf dt_second `seq`
    rnf dt_minute `seq`
    rnf dt_hour   `seq`
    rnf dt_day    `seq`
    rnf dt_month  `seq`
    rnf dt_year   `seq` ()

instance Show UTCDateTime where
  show dt = printf "\"%04d-%02d-%02d %02d:%02d:%02d\"" y (fromEnum mon) d h mi s where
    UTCDateTimeStruct{ dt_year   = Year y
                     , dt_month  = mon
                     , dt_day    = Day d
                     , dt_hour   = Hours h
                     , dt_minute = Minutes mi
                     , dt_second = Seconds s} = toStruct dt

instance Show UTCDateTimeNanos where
  show dt = printf "\"%04d-%02d-%02d %02d:%02d:%02d.%03d\"" y (fromEnum mon) d h mi s ms where
    UTCDateTimeStruct{ dt_year   = Year y
                     , dt_month  = mon
                     , dt_day    = Day d
                     , dt_hour   = Hours h
                     , dt_minute = Minutes mi
                     , dt_second = Seconds s
                     , dt_millis = Millis ms} = toStruct dt

dateTimeAsString :: DateTime dt UTCTimeZone => dt -> String
dateTimeAsString dt = printf "%04d-%02d-%02d %02d:%02d:%02d.%03d" y (fromEnum mon) d h mi s ms where
  UTCDateTimeStruct{ dt_year   = Year y
                   , dt_month  = mon
                   , dt_day    = Day d
                   , dt_hour   = Hours h
                   , dt_minute = Minutes mi
                   , dt_second = Seconds s
                   , dt_millis = Millis ms} = toStruct dt
-}

{-

class DateTimeMath a b where
  datePlus :: a -> b -> a

instance DateTimeMath UTCDateTime Second where
  UTCDateTime base `datePlus` Second sec = UTCDateTime $ base + fromIntegral sec

instance DateTimeMath UTCDateTime Minute where
  UTCDateTime base `datePlus` Minute m = UTCDateTime $ base + fromIntegral (m * 60)

instance DateTimeMath UTCDateTime Hour where
  UTCDateTime base `datePlus` Hour hr = UTCDateTime $ base + fromIntegral (hr * 60 * 60)

instance DateTimeMath UTCDateTime Day where
  UTCDateTime base `datePlus` Day d = UTCDateTime $ base + fromIntegral (d * 24 * 60 * 60)

instance DateTimeMath UTCDateTimeNanos Nanos where
  UTCDateTimeNanos (base, base_ns) `datePlus` Nanos add_ns = UTCDateTimeNanos (base + add_sec, (fromIntegral new_ns)) where
    (add_sec, new_ns) = ((fromIntegral base_ns) + add_ns) `divMod` 1000000000

instance DateTimeMath UTCDateTimeNanos Millis where
  UTCDateTimeNanos (base, base_ns) `datePlus` Millis add_ms = UTCDateTimeNanos (base + add_sec, (fromIntegral new_ns)) where
    -- The millis that come in are an Int64, so we do the math to split total millis into total seconds and millis since last second in Int64, then then truncate the remainder down to Int32
    (add_sec, new_ns) = ((fromIntegral base_ns) + add_ms * 1000000) `divMod` 1000000000

instance DateTimeMath UTCDateTimeNanos Second where
  UTCDateTimeNanos (base, ns) `datePlus` Second sec = UTCDateTimeNanos (base + fromIntegral sec, ns)

instance DateTimeMath UTCDateTimeNanos Minute where
  UTCDateTimeNanos (base, ns) `datePlus` Minute m = UTCDateTimeNanos (base + fromIntegral (m * 60), ns)

instance DateTimeMath UTCDateTimeNanos Hour where
  UTCDateTimeNanos (base, ns) `datePlus` Hour hr = UTCDateTimeNanos (base + fromIntegral (hr * 60 * 60), ns)

javaDateTimeToUTCDateTime :: Int64 -> UTCDateTime
javaDateTimeToUTCDateTime = UTCDateTime . (`quot` 1000)

foreign import ccall "time" c'time :: Ptr CTime -> IO CTime

getCurrentDateTime :: IO UTCDateTime
getCurrentDateTime = alloca (\ x -> c'time x >>= return . UTCDateTime . fromIntegral . fromEnum)

getCurrentDateTimeNanos :: IO UTCDateTimeNanos
getCurrentDateTimeNanos = getTimeOfDay >>= (\C'timeval { c'timeval'tv_sec = s,
                                                         c'timeval'tv_usec = us } -> return $ UTCDateTimeNanos (fromIntegral $ s, fromIntegral $ us * 1000))

today :: IO UTCDateTime
today = do
  tm <- getCurrentDateTime
  let today = DlM.set hour 0 tm
          >>= DlM.set minute 0
          >>= DlM.set second 0
  case today of
    Just val -> return $! val
    Nothing  -> error "conversion failed"

createUTCDateTime :: Second -> Minute -> Hour -> Day -> Month -> Year -> UTCDateTime
createUTCDateTime s m h d mon y = createDateTime s m h d mon y 0

createDateTime :: Second -> Minute -> Hour -> Day -> Month -> Year -> Int -> UTCDateTime
{-# INLINE createDateTime #-}
createDateTime (Second s) (Minute m) (Hour h) (Day d) mon (Year y) off =
  if y >= 1970 then UTCDateTime $ fromIntegral $ fromEnum ctime -- Note: There is an edgecase where the timezone off will push the date before 1970
  else error "createDateTime: Years before 1970 are not supported"
  where ctime :: CTime -- Apply the offset directly to the Hours/Minutes/Seconds because timegm ignores the offset member
        ctime = {-# SCC "ctime" #-}(convert $ C'tm (cint $ s - soff) (cint $ m - moff) (cint $ h - hoff) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = {-# SCC "cmon" #-}cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: (Integral a) => a -> CInt
        cint = {-# SCC "cint" #-}fromIntegral
        off32 = fromIntegral off
        (hoff, mofftot) = {-# SCC "hoff" #-}off32 `divMod` 3600
        (moff, soff) = {-# SCC "moff/soff" #-}mofftot `divMod` 60

createLocalDateTime :: Second -> Minute -> Hour -> Day -> Month -> Year -> IO UTCDateTime
createLocalDateTime (Second s) (Minute m) (Hour h) (Day d) mon (Year y) =
  if y >= 1970 then do
    ctime <- convertLocal tm
    return $ UTCDateTime $ fromIntegral $ fromEnum ctime
  -- Note: There is an edgecase where the local timezone offset will push the date before 1970
  else error "createLocalDateTime: Years before 1970 are not supported"
  where tm = C'tm (cint s) (cint m) (cint h) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr
        cmon = cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: Integral a => a -> CInt
        cint = fromIntegral
        convertLocal tm' = with tm' (\tm_ptr -> c'mktime tm_ptr)

createUTCDateTimeNanos :: Millis -> Second -> Minute -> Hour -> Day -> Month -> Year -> UTCDateTimeNanos
createUTCDateTimeNanos ms s m h d mon y = createDateTimeNanos ms s m h d mon y 0

createDateTimeNanos :: Millis -> Seconds -> Minutes -> Hours -> Day -> Month -> Year -> Int -> UTCDateTimeNanos
createDateTimeNanos (Millis ms) (Seconds s) (Minutes m) (Hours h) (Day d) mon (Year y) off =
  if y >= 1970 then UTCDateTimeNanos (fromIntegral $ fromEnum ctime, fromIntegral $ ms * 1000000)
  else error "createDateTimeNanos: Years before 1970 are not supported"
  where ctime :: CTime -- Apply the offset directly to the Hours/Minutes/Seconds because timegm ignores the offset member
        ctime = (convert $ C'tm (cint $ s - soff) (cint $ m - moff) (cint $ h - hoff) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = cint $ ((fromIntegral $ fromEnum mon) :: Int32) - 1
        cint :: (Integral a) => a -> CInt
        cint = fromIntegral
        off32 = fromIntegral off
        (hoff, mofftot) = off32 `divMod` 3600
        (moff, soff) = mofftot `divMod` 60

toHaskellLocalTime :: UTCDateTime -> HT.LocalTime
toHaskellLocalTime dt = HT.LocalTime (fromJust $ HT.fromGregorianValid (fromIntegral $ year dt) (fromEnum $ month dt) (fromIntegral $ day dt)) $
                                     HT.TimeOfDay (fromIntegral $ hour dt) (fromIntegral $ minute dt) (fromIntegral $ second dt)

fromHaskellLocalTime :: HT.LocalTime -> UTCDateTime
fromHaskellLocalTime (HT.LocalTime htday (HT.TimeOfDay h m s)) = let (y, mon, d) = {-# SCC "fromhlt_let" #-} HT.toGregorian htday
                                                                 in createUTCDateTime (Second $ round $ toRational s)
                                                                                      (Minute $ fromIntegral m)
                                                                                      (Hour $ fromIntegral h)
                                                                                      (Day $ fromIntegral d)
                                                                                      (toEnum mon)
                                                                                      (Year $ fromIntegral y)

toHaskellUTCTime :: UTCDateTime -> HT.UTCTime
toHaskellUTCTime dt = {-# SCC "toHaskellUTCTime1" #-} HT.UTCTime (fromJust $ HT.fromGregorianValid (fromIntegral $ year dt) (fromEnum $ month dt) (fromIntegral $ day dt)) $
                                 {-# SCC "toHaskellUTCTime1" #-} HT.secondsToDiffTime ((fromIntegral $ hour dt) * 3600 + (fromIntegral $ minute dt) * 60 + (fromIntegral $ second dt))

fromHaskellUTCTime :: HT.UTCTime -> UTCDateTime
fromHaskellUTCTime HT.UTCTime { utctDay=dt, utctDayTime=tm } =
  let (y, mon, d) = {-# SCC "toGregorian" #-}  HT.toGregorian dt
      ts = round $ toRational tm
      (h, remsec) = ts `divMod` 3600
      (m, s) = remsec `divMod` 60
  in {-# SCC "fromHaskellUTCTime" #-}  createUTCDateTime (Second s) (Minute m) (Hour h) (Day $ fromIntegral d) (toEnum mon) (Year $ fromIntegral y)

fromTimezone :: FilePath -> UTCDateTime -> UTCDateTime
fromTimezone tzPath dt = let  tz = {-# SCC "loading_olson" #-}  unsafePerformIO $ getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ tzPath)
                              in {-# SCC "fromTimezoneToUTC_in" #-} fromHaskellUTCTime $ localTimeToUTC' tz $ toHaskellLocalTime dt

toTimezone :: FilePath -> UTCDateTime -> UTCDateTime
toTimezone tzPath dt = let tz = {-# SCC "loading_olson" #-} unsafePerformIO $ getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ tzPath)
                              in fromHaskellLocalTime $ utcToLocalTime' tz $ toHaskellUTCTime dt

loadTimeZone :: String -> IO TimeZoneSeries
loadTimeZone tzPath = do
  getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ tzPath)

--fromTimezoneToUTC' :: TimeZoneSeries -> UTCDateTime -> UTCDateTime
--fromTimezoneToUTC' tz dt = fromHaskellUTCTime $ localTimeToUTC' tz $ toHaskellLocalTime dt

fromTimezoneToUTC' :: TimeZoneSeries -> UTCDateTime -> UTCDateTime
fromTimezoneToUTC' tz dt =
  let hlt = {-# SCC "step1" #-} toHaskellLocalTime dt
      utc = {-# SCC "step2" #-} localTimeToUTC' tz hlt
      ret = {-# SCC "step3" #-} fromHaskellUTCTime utc
  in  ret

fromUTCToTimezone' :: TimeZoneSeries -> UTCDateTime -> UTCDateTime
fromUTCToTimezone' tz dt = fromHaskellLocalTime $ utcToLocalTime' tz $ toHaskellUTCTime dt

addDateTime :: UTCDate -> UTCTime -> UTCDateTime
addDateTime (UTCDate d) (UTCTime t) = UTCDateTime (d + t)

floorToInterval :: Int -> UTCDateTimeNanos -> UTCDateTimeNanos
floorToInterval maxInterval (UTCDateTimeNanos (s, ns)) = UTCDateTimeNanos (s, ns `div` maxIntNs * maxIntNs) where
  maxIntNs = fromIntegral maxInterval * 1000000

addMillis :: UTCDateTimeNanos -> Millis -> UTCDateTimeNanos
addMillis (UTCDateTimeNanos (s, ns)) ms = UTCDateTimeNanos (s + s', fromIntegral ns') where
  (s', ns') = (fromIntegral ns + fromIntegral ms * 1000000) `divMod` 1000000000

deltaInMillis :: UTCDateTimeNanos -> UTCDateTimeNanos -> Millis
deltaInMillis (UTCDateTimeNanos (s1, ns1)) (UTCDateTimeNanos (s2, ns2)) = fromIntegral ((s1 - s2) * 1000000) + fromIntegral ((ns1 - ns2) `div` 1000)

extract :: (C'tm -> CInt) -> Int64 -> Int32
extract fn val = unsafePerformIO $ do
  let ctime = fromIntegral val
  y <- with ctime (\ ctime' -> alloca (\ tm -> do
    tm' <- c'gmtime_r ctime' tm
    tm'' <- peek tm'
    return $ fn tm''))
  return $! convert y

cT2T :: CTime -> UTCTime
cT2T = UTCTime . fromIntegral . fromEnum

cT2D :: CTime -> UTCDate
cT2D = UTCDate . fromIntegral . fromEnum

cT2DT :: CTime -> UTCDateTime
cT2DT = UTCDateTime . fromIntegral . fromEnum

cT2DTm :: CTime -> Int32 -> UTCDateTimeNanos
cT2DTm ct ns = UTCDateTimeNanos (fromIntegral $ fromEnum ct, ns)

instance Date UTCDate where
  century _                  = DlM.lens (\ _ -> return False) (\ _ _ -> return False) -- (Century 0
  day (UTCDate x)            = Day $ extract c'tm'tm_mday x
  dayOfWeek (UTCDate x)      = toEnum $ fromIntegral $ extract c'tm'tm_wday x
  dayOfYear (UTCDate x)      = Day $ extract c'tm'tm_yday x
  era _                      = Era 1 -- AD
  month (UTCDate x)          = toEnum $ fromIntegral $ extract c'tm'tm_mon x
  year (UTCDate x)           = Year $ extract c'tm'tm_year x
  yearOfCentury (UTCDate x)  = Year $ extract c'tm'tm_year x `mod` 100
  yearOfEra (UTCDate x)      = Year $ extract c'tm'tm_year x
  weekOfWeekyear (UTCDate _) = error "weekOfWeekyear nyi"
  weekyear (UTCDate _)       = error "weekyear nyi"

  -- withYear (UTCDate dt) (Year y) = cT2D $ withField (fromIntegral dt) (\x -> x { c'tm'tm_year = fromIntegral (y - 1900) })
  -- withMonth (UTCDate dt) m       = cT2D $ withField (fromIntegral dt) (\x -> x { c'tm'tm_mon = fromIntegral $ (fromEnum m - 1) })
  -- withDay (UTCDate dt) (Day d)   = cT2D $ withField (fromIntegral dt) (\x -> x { c'tm'tm_mday = fromIntegral d })

instance Time UTCTime where
  hour (UTCTime x)        = Hour $ extract c'tm'tm_hour x
  millisOfDay (UTCTime _) = error "millisOfDay nyi"
  millis (UTCTime _)      = Millis 0
  minuteOfDay (UTCTime x) = Minute $ extract c'tm'tm_min x + (extract c'tm'tm_hour x * 60)
  minute (UTCTime x)      = Minute $ extract c'tm'tm_min x
  secondOfDay (UTCTime _) = error "secondOfDay nyi"
  second (UTCTime x)      = Second $ extract c'tm'tm_sec x
  nanoseconds (UTCTime _) = Nanos 0

  -- withHour (UTCTime dt) (Hour h)     = cT2T $ withField (fromIntegral dt) (\x -> x { c'tm'tm_hour = fromIntegral h })
  -- withMinute (UTCTime dt) (Minute m) = cT2T $ withField (fromIntegral dt) (\x -> x { c'tm'tm_min = fromIntegral m })
  -- withSecond (UTCTime dt) (Second s) = cT2T $ withField (fromIntegral dt) (\x -> x { c'tm'tm_sec = fromIntegral s })

instance Date UTCDateTime where
  century _                      = Century 0
  day (UTCDateTime x)            = Day $ extract c'tm'tm_mday x
  dayOfWeek (UTCDateTime x)      = toEnum $ fromIntegral $ extract c'tm'tm_wday x
  dayOfYear (UTCDateTime x)      = Day $ extract c'tm'tm_yday x
  era _                          = Era 1 -- AD
  month (UTCDateTime x)          = toEnum $ fromIntegral $ 1 + extract c'tm'tm_mon x
  year (UTCDateTime x)           = Year $ 1900 + extract c'tm'tm_year x
  yearOfCentury (UTCDateTime x)  = Year $ extract c'tm'tm_year x `mod` 100
  yearOfEra (UTCDateTime x)      = Year $ 1900 + extract c'tm'tm_year x
  weekOfWeekyear (UTCDateTime _) = error "weekOfWeekyear nyi"
  weekyear (UTCDateTime _)       = error "weekyear nyi"

  -- withYear (UTCDateTime dt) (Year y) = cT2DT $ withField (fromIntegral dt) (\x -> x { c'tm'tm_year = fromIntegral (y - 1900) })
  -- withMonth (UTCDateTime dt) m       = cT2DT $ withField (fromIntegral dt) (\x -> x { c'tm'tm_mon = fromIntegral $ (fromEnum m - 1) })
  -- withDay (UTCDateTime dt) (Day d)   = cT2DT $ withField (fromIntegral dt) (\x -> x { c'tm'tm_mday = fromIntegral d })

instance Time UTCDateTime where
  hour (UTCDateTime x)        = Hour $ extract c'tm'tm_hour x
  millisOfDay (UTCDateTime _) = error "millisOfDay nyi"
  millis (UTCDateTime _)      = Millis 0
  minuteOfDay (UTCDateTime x) = Minute $ extract c'tm'tm_min x + (extract c'tm'tm_hour x * 60)
  minute (UTCDateTime x)      = Minute $ extract c'tm'tm_min x
  secondOfDay tm              = Second $ s + m * 60 + h * 3600
    where UTCDateTimeStruct { dt_second = Second s, dt_minute = Minute m, dt_hour = Hour h } = toStruct tm
  second (UTCDateTime x)      = Second $ extract c'tm'tm_sec x
  nanoseconds (UTCDateTime _) = Nanos 0

  -- withHour (UTCDateTime dt) (Hour h)     = cT2DT $ withField (fromIntegral dt) (\x -> x { c'tm'tm_hour = fromIntegral h })
  -- withMinute (UTCDateTime dt) (Minute m) = cT2DT $ withField (fromIntegral dt) (\x -> x { c'tm'tm_min = fromIntegral m })
  -- withSecond (UTCDateTime dt) (Second s) = cT2DT $ withField (fromIntegral dt) (\x -> x { c'tm'tm_sec = fromIntegral s })

instance Date UTCDateTimeNanos where
  century _                               = Century 0
  day (UTCDateTimeNanos (x,_))            = day (UTCDateTime x)
  dayOfWeek (UTCDateTimeNanos (x,_))      = dayOfWeek (UTCDateTime x)
  dayOfYear (UTCDateTimeNanos (x,_))      = dayOfYear (UTCDateTime x)
  era _                                   = Era 1 -- AD
  month (UTCDateTimeNanos (x,_))          = month (UTCDateTime x)
  year (UTCDateTimeNanos (x,_))           = year (UTCDateTime x)
  yearOfCentury (UTCDateTimeNanos (x,_))  = yearOfCentury (UTCDateTime x)
  yearOfEra (UTCDateTimeNanos (x,_))      = yearOfEra (UTCDateTime x)
  weekOfWeekyear (UTCDateTimeNanos (x,_)) = weekOfWeekyear (UTCDateTime x)
  weekyear (UTCDateTimeNanos (x,_))       = weekyear (UTCDateTime x)

  -- withYear (UTCDateTimeNanos (dt, ms)) (Year y) = cT2DTm (withField (fromIntegral dt) (\x -> x { c'tm'tm_year = fromIntegral (y - 1900) })) ms
  -- withMonth (UTCDateTimeNanos (dt, ms)) m       = cT2DTm (withField (fromIntegral dt) (\x -> x { c'tm'tm_mon = fromIntegral $ (fromEnum m - 1) })) ms
  -- withDay (UTCDateTimeNanos (dt, ms)) (Day d)   = cT2DTm (withField (fromIntegral dt) (\x -> x { c'tm'tm_mday = fromIntegral d })) ms

instance Time UTCDateTimeNanos where
  hour (UTCDateTimeNanos (x,_))        = hour (UTCDateTime x)
  millisOfDay (UTCDateTimeNanos (x,n)) = Millis $ fromIntegral (getSecond (secondOfDay (UTCDateTime x))) * 1000 + (fromIntegral n `div` 1000000)
  millis (UTCDateTimeNanos (_,x))      = Millis $ fromIntegral x `div` 1000000
  minuteOfDay (UTCDateTimeNanos (x,_)) = minuteOfDay (UTCDateTime x)
  minute (UTCDateTimeNanos (x,_))      = minute (UTCDateTime x)
  secondOfDay (UTCDateTimeNanos (x,_)) = secondOfDay (UTCDateTime x)
  second (UTCDateTimeNanos (x,_))      = second (UTCDateTime x)
  nanoseconds (UTCDateTimeNanos (_,x)) = Nanos $ fromIntegral x `mod` 1000000

  -- withHour (UTCDateTimeNanos (dt, ms)) (Hour h)     = cT2DTm (withField (fromIntegral dt) (\x -> x { c'tm'tm_hour = fromIntegral h })) ms
  -- withMinute (UTCDateTimeNanos (dt, ms)) (Minute m) = cT2DTm (withField (fromIntegral dt) (\x -> x { c'tm'tm_min = fromIntegral m })) ms
  -- withSecond (UTCDateTimeNanos (dt, ms)) (Second s) = cT2DTm (withField (fromIntegral dt) (\x -> x { c'tm'tm_sec = fromIntegral s })) ms


instance DateTime UTCDateTime where
  toStruct (UTCDateTime dt) = UTCDateTimeStruct (Year $ (fromIntegral $ c'tm'tm_year tm) + 1900)
                                                (toEnum $ (fromIntegral $ c'tm'tm_mon tm) + 1)
                                                (Day $ fromIntegral $ c'tm'tm_mday tm)
                                                (Hour $ fromIntegral $ c'tm'tm_hour tm)
                                                (Minute $ fromIntegral $ c'tm'tm_min tm)
                                                (Second $ fromIntegral $ c'tm'tm_sec tm)
                                                0
    where tm = (convert $ ((fromIntegral dt) :: CTime)) :: C'tm

  fromStruct UTCDateTimeStruct { dt_second=s, dt_minute=m, dt_hour=h, dt_day=d, dt_month=mon, dt_year=y } =
    createUTCDateTime s m h d mon y

instance DateTime UTCDateTimeNanos where
  toStruct (UTCDateTimeNanos (dt, ns)) = UTCDateTimeStruct (Year $ (fromIntegral $ c'tm'tm_year tm) + 1900)
                                                           (toEnum $ (fromIntegral $ c'tm'tm_mon tm) + 1)
                                                           (Day $ fromIntegral $ c'tm'tm_mday tm)
                                                           (Hours $ fromIntegral $ c'tm'tm_hour tm)
                                                           (Minutes $ fromIntegral $ c'tm'tm_min tm)
                                                           (Seconds $ fromIntegral $ c'tm'tm_sec tm)
                                                           (Millis $ fromIntegral $ ns `div` 1000000)
    where tm = (convert $ ((fromIntegral dt) :: CTime)) :: C'tm

  fromStruct UTCDateTimeStruct { dt_millis=ms, dt_second=s, dt_minute=m, dt_hour=h, dt_day=d, dt_month=mon, dt_year=y } =
    createUTCDateTimeNanos ms s m h d mon y

instance Chronology UTCTime
instance Chronology UTCDate
instance Chronology UTCDateTime
instance Chronology UTCDateTimeNanos
-}
