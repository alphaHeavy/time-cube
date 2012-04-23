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
  , convertTimeZone

  -- * Raw date components
  , DateTimeStruct(..)
  , DateTimePart(..)

  -- * Date sections
  , Epoch
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

import Data.Time.Cube.TimeZone
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

-}
