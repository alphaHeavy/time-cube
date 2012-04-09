{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Library that provides 4 corner simultaneous 24 hour Days that occur within a single 4 corner rotation of Earth.
--
module AlphaHeavy.Time
  ( DateTime(..)
  , DateTimeLensT(..)
  , TimeZone(..)
  , TimeZoneOffset(..)

  -- * Raw date components
  , DateTimeComponents(..)
  , DateTimeStruct(..)
  , DateTimePart(..)

  -- * System time
  , getCurrentUnixTime
  , getCurrentUnixTimeNanos

  -- * Lens accessors
  -- ** Pure
  , AlphaHeavy.Time.get
  , AlphaHeavy.Time.set
  , AlphaHeavy.Time.modify

  -- ** Monadic
  , AlphaHeavy.Time.getM
  , AlphaHeavy.Time.setM
  , AlphaHeavy.Time.modifyM

  -- * Date storage
  , UnixTime(..)
  , UnixTimeNanos(..)
  , JavaTime
  , JulianDate

  -- * Date sections
  , Year(..)
  , Month(..)
  , MonthOfYear(..)
  , Week(..)
  , Day(..)
  , DayOfWeek(..)
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
  , DateTimeMath(..)

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
import Data.Data
import Data.Maybe (fromJust)
import qualified Data.Time as HT
-- import Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile)
-- import Data.Time.LocalTime.TimeZone.Series (localTimeToUTC',TimeZoneSeries, utcToLocalTime')
import Foreign (Ptr,nullPtr,alloca,with,peek)
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import Prelude hiding ((.), id)

data DateTimeStruct = DateTimeStruct
  { dt_year   :: Year
  , dt_month  :: Month
  , dt_day    :: Day
  , dt_hour   :: Hour
  , dt_minute :: Minute
  , dt_second :: Second
  , dt_nanos  :: Nano
  }

deriving instance Show DateTimeStruct

newtype Era     = Era     {getEra     :: Int32}
newtype Century = Century {getCentury :: Int32}
newtype Week    = Weeks   {getWeek    :: Int32}

newtype TimeZoneOffset = TimeZoneOffset {getTimeZoneOffset :: Int32}
deriving instance Show   TimeZoneOffset
deriving instance NFData TimeZoneOffset

newtype Year = Years {getYear :: Int32}
deriving instance Read   Year
deriving instance Show   Year
deriving instance Num    Year
deriving instance NFData Year

newtype Month = Months {getMonth :: Int32}
deriving instance Read   Month
deriving instance Show   Month
deriving instance Num    Month
deriving instance NFData Month

data MonthOfYear
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

deriving instance Eq     MonthOfYear
deriving instance Ord    MonthOfYear
deriving instance Read   MonthOfYear
deriving instance Show   MonthOfYear

instance NFData MonthOfYear where
  rnf x = x `seq` ()

instance Enum MonthOfYear where
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

instance Bounded MonthOfYear where
  minBound = January
  maxBound = December

newtype Day = Day {getDay :: Int32}
deriving instance Read   Day
deriving instance Show   Day
deriving instance Num    Day
deriving instance NFData Day

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

newtype Hour = Hours {getHour :: Int32}
deriving instance Read   Hour
deriving instance Show   Hour
deriving instance Num    Hour
deriving instance NFData Hour

newtype Minute = Minutes {getMinute :: Int32}
deriving instance Read   Minute
deriving instance Show   Minute
deriving instance Num    Minute
deriving instance NFData Minute

newtype Second = Seconds {getSecond :: Int32}
deriving instance Read   Second
deriving instance Show   Second
deriving instance Num    Second
deriving instance NFData Second

newtype Milli = Millis {getMillis :: Int64}
deriving instance Read   Milli
deriving instance Show   Milli
deriving instance Num    Milli
deriving instance NFData Milli

newtype Nano = Nanos {getNanos :: Int64}
deriving instance Read   Nano
deriving instance Show   Nano
deriving instance Num    Nano
deriving instance NFData Nano

newtype Pico = Picos {getPicos :: Int64}
deriving instance Read   Pico
deriving instance Show   Pico
deriving instance Num    Pico
deriving instance NFData Pico

data TimeZone = UTC | LocalTime -- NamedTimeZone String

newtype UnixTime (t :: TimeZone)      = UnixTime Int64 deriving (Num,Ord,Eq,NFData)
deriving instance Show (UnixTime tz)

newtype UnixTimeNanos (t :: TimeZone) = UnixTimeNanos (Int64, Int32) deriving (Ord,Eq,NFData)
deriving instance Show (UnixTimeNanos tz)

newtype JavaTime (t :: TimeZone)      = JavaTime Int64

newtype JulianDate (t :: TimeZone)    = JulianDate Int64

dateTimeLens
  :: (f -> StateT c m a)
  -> (a -> f -> StateT c m f)
  -> DateTimeLensT m c f a
dateTimeLens g s = DateTimeLensT (A.lens (Kleisli g) (Kleisli (uncurry s)))

class DateTime f where
  -- |
  -- The timezone associated with this type
  type DateTimeZone f :: TimeZone

  -- |
  -- The natural components of the time.
  -- For a regular datetime this may represent days\/minutes\/hours\/etc.
  -- For durations it will be in seconds or an equivalent unit
  data DateTimeComponents f :: * -- (#)

  -- |
  -- Unpack a time into its components
  unpack :: f -> DateTimeComponents f

  -- |
  -- Repack a time from its components
  pack   :: DateTimeComponents f -> f

class DateTimePart c f a where
  -- |
  -- A lens from a time into a time component
  datePart :: (Functor m, Monad m) => DateTimeLensT m c f a
  datePart = dateTimeLens dtg dts

  dtg :: (Functor m, Monad m) => f -> StateT c m a
  dts :: (Functor m, Monad m) => a -> f -> StateT c m f

newtype DateTimeLensT m c a b = DateTimeLensT{unDateTimeLens :: A.Lens (Kleisli (StateT c m)) a b}

deriving instance Monad m => Category (DateTimeLensT m c)

runDateTimeLensT
  :: (Monad m, DateTime f)
  => Kleisli (StateT (DateTimeComponents f) m) f a
  -> f
  -> m a
runDateTimeLensT l f = evalStateT (runKleisli l f) (unpack f)

getM
  :: (Functor m, Monad m, DateTime f)
  => DateTimeLensT m (DateTimeComponents f) f a
  -> f
  -> m a
getM = runDateTimeLensT . A.get . unDateTimeLens

setM
  :: (Functor m, Monad m, DateTime f)
  => DateTimeLensT m (DateTimeComponents f) f a
  -> a
  -> f
  -> m f
setM (DateTimeLensT l) v = runDateTimeLensT (A.set l . arr (v,))

modifyM
  :: (Functor m, Monad m, DateTime f)
  => DateTimeLensT m (DateTimeComponents f) f a
  -> (a -> a)
  -> f
  -> m f
modifyM (DateTimeLensT l) v = runDateTimeLensT (A.modify l . arr (arr v,))

get
  :: DateTime f
  => DateTimeLensT Identity (DateTimeComponents f) f a
  -> f
  -> a
get l = runIdentity . getM l

set
  :: DateTime f
  => DateTimeLensT Identity (DateTimeComponents f) f a
  -> a
  -> f
  -> f
set l v = runIdentity . setM l v

modify
  :: DateTime f
  => DateTimeLensT Identity (DateTimeComponents f) f a
  -> (a -> a)
  -> f
  -> f
modify l v = runIdentity . modifyM l v

instance DateTime HT.UTCTime where
  type DateTimeZone HT.UTCTime = 'UTC
  newtype DateTimeComponents HT.UTCTime = UTCTimeComponents{unUTCTimeComponents :: HT.UTCTime}
  unpack = UTCTimeComponents
  pack   = unUTCTimeComponents

instance DateTimePart (DateTimeComponents HT.UTCTime) HT.UTCTime Day where
  dtg = return . Day . fromIntegral . HT.toModifiedJulianDay . HT.utctDay
  dts val s = return s{HT.utctDay = HT.ModifiedJulianDay . fromIntegral $ getDay val}

instance DateTimePart (DateTimeComponents HT.UTCTime) Day Second where
  dtg _ = fmap (Seconds . truncate) $ gets (HT.utctDayTime . unUTCTimeComponents)
  dts val s = do
    State.modify $ \ (UTCTimeComponents s) -> UTCTimeComponents s{HT.utctDayTime = realToFrac $ getSecond val}
    return s

blah x = truncate (x * 1000000000000)

instance DateTimePart (DateTimeComponents HT.UTCTime) Day Pico where
  dtg _ = fmap (Picos . blah) $ gets (HT.utctDayTime . unUTCTimeComponents)
  -- dts val s = do
    -- State.modify $ \ s -> s{HT.utctDayTime = realToFrac $ getSecond val}
    -- return s

instance DateTime (UnixTimeNanos tz) where
  type DateTimeZone (UnixTimeNanos tz) = tz
  newtype DateTimeComponents (UnixTimeNanos tz) = UnixTimeNanosComponents{unUnixTimeNanosComponents :: DateTimeStruct}
  unpack = UnixTimeNanosComponents . unpackUnixTimeNanos
  pack   = packUnixTimeNanos . unUnixTimeNanosComponents

{-
instance DateTimePart DateTimeStruct b Year where
  dtg _     = State.gets dt_year
  dts val _ = do
    s <- State.get
    let s' = s{dt_year = val}
    State.put s'
    return $! pack s'
-}

instance DateTimePart DateTimeStruct (UnixTimeNanos tz) Year where
  dtg _     = State.gets dt_year
  dts val _ = do
    s <- State.get
    let s' = s{dt_year = val}
    State.put s'
    return $! pack (UnixTimeNanosComponents s')

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) (UnixTimeNanos tz) Year where
  dtg _   = State.gets (dt_year . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_year = val}
    State.put (UnixTimeNanosComponents s')
    return $! pack (UnixTimeNanosComponents s')

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) (UnixTimeNanos tz) MonthOfYear where
  dtg _   = fmap (toEnum . fromIntegral . getMonth) $ State.gets (dt_month . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_month = Months . fromIntegral $ fromEnum val}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Year Month where
  dtg _   = State.gets (dt_month . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_month = val}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Month Day where
  dtg _   = State.gets (dt_day . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_day = val}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Day Hour where
  dtg _   = State.gets (dt_hour . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_hour = val}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Hour Minute where
  dtg _   = State.gets (dt_minute . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_minute = val}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Minute Second where
  dtg _   = State.gets (dt_second . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_second = val}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Second Milli where
  dtg _   = fmap (\ ns -> Millis (ns `div` 1000000)) $ State.gets (getNanos . dt_nanos . unUnixTimeNanosComponents)
  dts (Millis val) x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_nanos = Nanos (val * 1000000)}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Second Nano where
  dtg _   = State.gets (dt_nanos . unUnixTimeNanosComponents)
  dts val x = do
    UnixTimeNanosComponents s <- State.get
    let s' = s{dt_nanos = val}
    State.put (UnixTimeNanosComponents s')
    return $ undefined -- dt_year . unpack . pack $ UnixTimeNanosComponents s'

{-
instance DateTime (UnixTime tz) where
  type DateTimeZone (UnixTime tz) = tz
  newtype DateTimeComponents (UnixTime tz) = UnixTimeComponents{unUnixTimeComponents :: DateTimeStruct}
  unpack   = unpackUnixTime
  pack     = packUnixTime

instance DateTime HT.LocalTime where
  type DateTimeZone HT.LocalTime = 'LocalTime
  newtype DateTimeComponents HT.LocalTime = LocalTimeComponents{unLocalTimeComponents :: DateTimeStruct}
  -- unpack = undefined
  -- pack   = undefined
-}

unpackUnixTimeNanos :: UnixTimeNanos tz -> DateTimeStruct
unpackUnixTimeNanos (UnixTimeNanos (s, ns)) = val' where
  val'   = val{dt_nanos = nano}
  val    = unpackUnixTime (UnixTime s)
  nano   = Nanos   . fromIntegral $ ns

packUnixTimeNanos :: DateTimeStruct -> UnixTimeNanos tz
packUnixTimeNanos dts@DateTimeStruct{..} = val' where
  val'       = UnixTimeNanos (s, fromIntegral ns)
  UnixTime s = packUnixTime dts
  Nanos ns   = dt_nanos

unpackUnixTime :: UnixTime tz -> DateTimeStruct
unpackUnixTime (UnixTime s) = val where
  val    = DateTimeStruct year month day hour minute second 0
  year   = Years   . fromIntegral $ c'tm'tm_year tm + 1900
  month  = Months  . fromIntegral $ c'tm'tm_mon  tm + 1
  day    = Day     . fromIntegral $ c'tm'tm_mday tm
  hour   = Hours   . fromIntegral $ c'tm'tm_hour tm
  minute = Minutes . fromIntegral $ c'tm'tm_min  tm
  second = Seconds . fromIntegral $ c'tm'tm_sec  tm
  tm     = convert (fromIntegral s :: CTime)

packUnixTime :: DateTimeStruct -> UnixTime tz
packUnixTime DateTimeStruct{..} = UnixTime (fromIntegral val') where
  val' :: Int
  val' = convert val
  val :: CTime
  val = convert C'tm
    { c'tm'tm_year   = fromIntegral $ getYear   dt_year  - 1900
    , c'tm'tm_mon    = fromIntegral $ getMonth  dt_month - 1
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

year
  :: (Functor m, Monad m, DateTimePart c f Year)
  => DateTimeLensT m c f Year
year = datePart

month
  :: (Functor m, Monad m, DateTimePart c f Month)
  => DateTimeLensT m c f Month
month = datePart

monthOfYear
  :: (Functor m, Monad m, DateTimePart c f MonthOfYear)
  => DateTimeLensT m c f MonthOfYear
monthOfYear = datePart

week
  :: (Functor m, Monad m, DateTimePart c f Week)
  => DateTimeLensT m c f Week
week = datePart

dayOfWeek
  :: (Functor m, Monad m, DateTimePart c f DayOfWeek)
  => DateTimeLensT m c f DayOfWeek
dayOfWeek = datePart

day
  :: (Functor m, Monad m, DateTimePart c f Day)
  => DateTimeLensT m c f Day
day = datePart

hour
  :: (Functor m, Monad m, DateTimePart c f Hour)
  => DateTimeLensT m c f Hour
hour = datePart

minute
  :: (Functor m, Monad m, DateTimePart c f Minute)
  => DateTimeLensT m c f Minute
minute = datePart

second
  :: (Functor m, Monad m, DateTimePart c f Second)
  => DateTimeLensT m c f Second
second = datePart

milli
  :: (Functor m, Monad m, DateTimePart c f Milli)
  => DateTimeLensT m c f Milli
milli = datePart

nano
  :: (Functor m, Monad m, DateTimePart c f Nano)
  => DateTimeLensT m c f Nano
nano = datePart

pico
  :: (Monad m, Functor m, DateTimePart c f Pico)
  => DateTimeLensT m c f Pico
pico = datePart

timeZoneOffset
  :: (Functor m, Monad m, DateTimePart c f TimeZoneOffset)
  => DateTimeLensT m c f TimeZoneOffset
timeZoneOffset = datePart

getCurrentUnixTime :: IO (UnixTime 'UTC)
getCurrentUnixTime = fmap mk getCurrentUnixTimeNanos where
  mk (UnixTimeNanos (s, _)) = UnixTime s

getCurrentUnixTimeNanos :: IO (UnixTimeNanos 'UTC)
getCurrentUnixTimeNanos = do
  C'timeval{c'timeval'tv_sec = sec, c'timeval'tv_usec = ms} <- getTimeOfDay
  return $! UnixTimeNanos (fromIntegral sec, fromIntegral (ms * 1000))

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

class DateTimeMath a b where
  type DateTimeMathResult a b :: *
  datePlus :: a -> b -> DateTimeMathResult a b

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

instance Time UTCTime where
  hour (UTCTime x)        = Hour $ extract c'tm'tm_hour x
  millisOfDay (UTCTime _) = error "millisOfDay nyi"
  millis (UTCTime _)      = Millis 0
  minuteOfDay (UTCTime x) = Minute $ extract c'tm'tm_min x + (extract c'tm'tm_hour x * 60)
  minute (UTCTime x)      = Minute $ extract c'tm'tm_min x
  secondOfDay (UTCTime _) = error "secondOfDay nyi"
  second (UTCTime x)      = Second $ extract c'tm'tm_sec x
  nanoseconds (UTCTime _) = Nanos 0

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
-}
