{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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

  -- * Time conversions
  , convertDateTime
  , convertDateTimeZone

  -- * Raw date components
  , DateTimeStruct(..)
  ) where

import Data.Time.Cube.TM
import Data.Time.Cube.Lens
import Data.Time.Cube.Types
import Control.DeepSeq
import Control.Monad.State.Lazy as State
import Data.Convertible
import Data.Int
import Foreign (nullPtr)
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

getCurrentUnixTime :: IO (UnixTime 'UTC)
getCurrentUnixTime = fmap mk getCurrentUnixTimeNanos where
  mk (UnixTimeNanos (s, _)) = UnixTime s

getCurrentUnixTimeNanos :: IO (UnixTimeNanos 'UTC)
getCurrentUnixTimeNanos = do
  C'timeval{c'timeval'tv_sec = sec, c'timeval'tv_usec = ms} <- getTimeOfDay
  return $! UnixTimeNanos (fromIntegral sec, fromIntegral (ms * 1000))

