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
module Data.Time.Cube
  ( DateTime(..)
  , TimeZone(..)
  , TimeZoneOffset(..)

  -- * Date storage
  , UnixTime(..)
  , UnixTimeNanos(..)
  -- , JavaTime
  -- , JulianDate

  -- * System time
  , getCurrentUnixTime
  , getCurrentUnixTimeNanos

  -- * Time conversions
  , convertDateTime
  , convertDateTimeZone

  -- * Raw date components
  , DateTimeComponents
  , DateTimeStruct(..)
  , DateTimePart(..)
  -- , datePart

  -- * Lens accessors
  -- ** Pure
  -- , Data.Time.Cube.get
  -- , Data.Time.Cube.set
  -- , Data.Time.Cube.modify

  -- ** Monadic (currently not very monadic...)
  -- , Data.Time.Cube.getM
  -- , Data.Time.Cube.getM2
  -- , Data.Time.Cube.setM
  -- , Data.Time.Cube.modifyM

  -- * Helper lenses
  -- , epoch
  -- , era
  -- , century
  -- , year
  -- , month
  -- , monthOfYear
  -- , week
  -- , day
  -- , dayOfWeek
  -- , hour
  -- , minute
  -- , second
  -- , milli
  -- , nano
  -- , pico

  -- , timeZoneOffset

  -- * Date sections
  , Era(..)
  , Century(..)
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

  -- * Durations
  , Duration
  ) where

import Prelude hiding ((.), id)

import Data.Time.Cube.Types
import Data.Time.Cube.UnixTime

{-
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
