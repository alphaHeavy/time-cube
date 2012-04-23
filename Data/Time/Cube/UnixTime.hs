{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
module Data.Time.Cube.UnixTime
  ( -- * Date storage
    UnixTime(..)
  , UnixTimeNanos(..)

  -- * System time
  , getCurrentUnixTime
  , getCurrentUnixTimeNanos

  -- * Raw date components
  , DateTimeStruct(..)

  -- * Legacy helper functions
  , floorToInterval
  , addMillis
  , deltaInMillis
  , createUTCDateTime
  , createDateTime
  , createLocalDateTime
  , createUTCDateTimeNanos
  , createDateTimeNanos
  ) where

import Data.Time.Cube.TM
import Data.Time.Cube.Types
import Control.DeepSeq
import Control.Monad.State.Lazy as State
import Data.Int
import Foreign (nullPtr, with)
import Foreign.C.Types
import Control.Category ((.))
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

newtype UnixTime (t :: TimeZone)      = UnixTime Int64 deriving (Num,Ord,Eq,NFData)
deriving instance Show (UnixTime tz)

newtype UnixTimeNanos (t :: TimeZone) = UnixTimeNanos (Int64, Int32) deriving (Ord,Eq,NFData)
deriving instance Show (UnixTimeNanos tz)

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

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) (UnixTimeNanos tz) Second where
  dtg (UnixTimeNanos (s, _)) = return $! Seconds (fromIntegral s)
  dts val x = undefined

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

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Day Minute where
  dtg _ = do
    h <- State.gets (getHour . dt_hour . unUnixTimeNanosComponents)
    m <- State.gets (getMinute . dt_minute . unUnixTimeNanosComponents)
    return $! Minutes ((h * 60) + m)

  dts val x = do
    undefined

instance DateTimePart (DateTimeComponents (UnixTimeNanos tz)) Day Second where
  dtg _ = do
    h <- State.gets (getHour   . dt_hour   . unUnixTimeNanosComponents)
    m <- State.gets (getMinute . dt_minute . unUnixTimeNanosComponents)
    s <- State.gets (getSecond . dt_second . unUnixTimeNanosComponents)
    return $! Seconds ((h * 60 * 60) + (m * 60) + s)

  dts val x = do
    undefined

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
  tm     = convertTime (fromIntegral s :: CTime)

packUnixTime :: DateTimeStruct -> UnixTime tz
packUnixTime DateTimeStruct{..} = UnixTime (fromIntegral val') where
  val' :: Int
  val' = let CTime x = val in fromIntegral x
  val :: CTime
  val = convertTime C'tm
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

getCurrentUnixTime :: IO (UnixTime 'UTC)
getCurrentUnixTime = fmap mk getCurrentUnixTimeNanos where
  mk (UnixTimeNanos (s, _)) = UnixTime s

getCurrentUnixTimeNanos :: IO (UnixTimeNanos 'UTC)
getCurrentUnixTimeNanos = do
  C'timeval{c'timeval'tv_sec = sec, c'timeval'tv_usec = ms} <- getTimeOfDay
  return $! UnixTimeNanos (fromIntegral sec, fromIntegral (ms * 1000))


floorToInterval :: Int -> UnixTimeNanos tz -> UnixTimeNanos tz
floorToInterval maxInterval (UnixTimeNanos (s, ns)) = UnixTimeNanos (s, ns `div` maxIntNs * maxIntNs) where
  maxIntNs = fromIntegral maxInterval * 1000000

addMillis :: UnixTimeNanos tz -> Milli -> UnixTimeNanos tz
addMillis (UnixTimeNanos (s, ns)) (Millis ms) = UnixTimeNanos (s + s', fromIntegral ns') where
  (s', ns') = (fromIntegral ns + fromIntegral ms * 1000000) `divMod` 1000000000

deltaInMillis :: UnixTimeNanos tz -> UnixTimeNanos tz -> Milli
deltaInMillis (UnixTimeNanos (s1, ns1)) (UnixTimeNanos (s2, ns2)) = fromIntegral ((s1 - s2) * 1000000) + fromIntegral ((ns1 - ns2) `div` 1000)

createUTCDateTime :: Second -> Minute -> Hour -> Day -> Month -> Year -> UnixTime 'UTC
createUTCDateTime s m h d mon y = let UnixTime x = createDateTime s m h d mon y 0 in UnixTime x

createDateTime :: Second -> Minute -> Hour -> Day -> Month -> Year -> Int -> UnixTime 'LocalTime
{-# INLINE createDateTime #-}
createDateTime (Seconds s) (Minutes m) (Hours h) (Day d) mon (Years y) off =
  if y >= 1970 then UnixTime $ fromIntegral (fromEnum ctime) -- Note: There is an edgecase where the timezone off will push the date before 1970
  else error "createDateTime: Years before 1970 are not supported"
  where ctime :: CTime -- Apply the offset directly to the Hours/Minutes/Seconds because timegm ignores the offset member
        ctime = {-# SCC "ctime" #-}(convertTime $ C'tm (cint $ s - soff) (cint $ m - moff) (cint $ h - hoff) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = {-# SCC "cmon" #-}cint $ (getMonth mon) - 1
        cint :: (Integral a) => a -> CInt
        cint = {-# SCC "cint" #-}fromIntegral
        off32 = fromIntegral off
        (hoff, mofftot) = {-# SCC "hoff" #-}off32 `divMod` 3600
        (moff, soff) = {-# SCC "moff/soff" #-}mofftot `divMod` 60

createLocalDateTime :: Second -> Minute -> Hour -> Day -> Month -> Year -> IO (UnixTime 'LocalTime)
createLocalDateTime (Seconds s) (Minutes m) (Hours h) (Day d) mon (Years y) =
  if y >= 1970 then do
    ctime <- convertLocal tm
    return $! UnixTime $ fromIntegral (fromEnum ctime)
  -- Note: There is an edgecase where the local timezone offset will push the date before 1970
  else error "createLocalDateTime: Years before 1970 are not supported"
  where tm = C'tm (cint s) (cint m) (cint h) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr
        cmon = cint $ (getMonth mon) - 1
        cint :: Integral a => a -> CInt
        cint = fromIntegral
        convertLocal tm' = with tm' (\tm_ptr -> c'mktime tm_ptr)

createUTCDateTimeNanos :: Milli -> Second -> Minute -> Hour -> Day -> Month -> Year -> UnixTimeNanos 'UTC
createUTCDateTimeNanos ms s m h d mon y = createDateTimeNanos ms s m h d mon y 0

createDateTimeNanos :: Milli -> Second -> Minute -> Hour -> Day -> Month -> Year -> Int -> UnixTimeNanos 'UTC
createDateTimeNanos (Millis ms) (Seconds s) (Minutes m) (Hours h) (Day d) mon (Years y) off =
  if y >= 1970 then UnixTimeNanos (fromIntegral $ fromEnum ctime, fromIntegral $ ms * 1000000)
  else error "createDateTimeNanos: Years before 1970 are not supported"
  where ctime :: CTime -- Apply the offset directly to the Hours/Minutes/Seconds because timegm ignores the offset member
        ctime = (convertTime $ C'tm (cint $ s - soff) (cint $ m - moff) (cint $ h - hoff) (cint d) cmon (cint $ y - 1900) 0 0 (-1) 0 nullPtr) :: CTime
        cmon = cint $ (getMonth mon) - 1
        cint :: (Integral a) => a -> CInt
        cint = fromIntegral
        off32 = fromIntegral off
        (hoff, mofftot) = off32 `divMod` 3600
        (moff, soff) = mofftot `divMod` 60

