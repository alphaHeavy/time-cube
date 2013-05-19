{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AlphaHeavy.Time.Zones
  ( HasUtc(..)
  ) where

import Control.Lens
import Data.Proxy (Proxy)
import qualified Data.Time.LocalTime as TZ
import Data.Time.LocalTime.TimeZone.Series
import qualified Data.Time.LocalTime.TimeZone.Olson as Olson
import GHC.TypeLits
import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX

import AlphaHeavy.Time.Calendar.Types
import AlphaHeavy.Time.Types

import Debug.Trace

-- no accurate to sub minute :-(
loadTimeZone :: String -> Map (UnixDateTime 'UTC cal) (Minute cal)
loadTimeZone tzPath = unsafePerformIO $ do
  TimeZoneSeries{tzsTransitions = series} <- Olson.getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ tzPath)
  let conv (t, tz) = (UnixDateTime . floor $ utcTimeToPOSIXSeconds t, Minute . fromIntegral $ TZ.timeZoneMinutes tz)
  return . Map.fromList . fmap conv $ series

class HasUtc a where
  utc :: SingI tz => Iso' (a ('NamedZone tz) (cal :: Calendar)) (a 'UTC cal)

  namedZone :: SingI tz => Proxy tz -> Iso' (a 'UTC cal) (a ('NamedZone tz) cal)
  namedZone _ = from utc

timeZoneName :: forall dt tz r cal . SingI tz => (r -> dt ('NamedZone tz) cal) -> String
timeZoneName _ = fromSing (sing :: Sing tz)

instance HasUtc UnixDateTime where
  utc = iso s g where
    m = loadTimeZone $ timeZoneName g
    g t@(UnixDateTime secs)
      | Just (_, v) <- Map.lookupGE t m
      = traceShow v (UnixDateTime (secs + (fromIntegral v * 60)))

    s (UnixDateTime secs)
      | (lm, _)     <- Map.split (UnixDateTime secs) m
      , Just (_, v) <- Map.lookupGE (UnixDateTime secs) m
      = trace "error ...." (UnixDateTime (secs - (fromIntegral v * 60)))
