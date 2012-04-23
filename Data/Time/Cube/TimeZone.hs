{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Time.Cube.TimeZone
  ( convertTimeZone
  , convertDateTimeZone
  ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Time.Cube.Types
import Data.Time.Cube.UnixTime

-- class Monad m => TimeZoneSeries2 m (tz :: TimeZone) where
  -- convertToUTC   :: UnixTimeNanos tz -> m (UnixTimeNanos 'UTC)
  -- convertFromUTC :: UnixTimeNanos 'UTC -> m (UnixTimeNanos tz)

class Monad m => TimeZoneSeries2 (from :: TimeZone) m (to :: TimeZone) where
  convertTimeZone :: UnixTimeNanos from -> m (UnixTimeNanos to)

getLocalTimeZone :: Monad m => m String
getLocalTimeZone = return "US/Pacific"

instance MonadIO m => TimeZoneSeries2 'UTC m 'LocalTime where
  convertTimeZone t = do
    localTimeZone <- liftIO getLocalTimeZone
    case localTimeZone of
      "US/Eastern" -> do
        -- UnixTimeNanos (s, ns) :: UnixTimeNanos '(TimeZone "US/Pacific") <- convertTimeZone t
        UnixTimeNanos (s, ns) :: UnixTimeNanos 'USEastern <- convertTimeZone t
        return $! UnixTimeNanos (s, ns) 

      "US/Pacific" -> do
        -- UnixTimeNanos (s, ns) :: UnixTimeNanos '(TimeZone "US/Pacific") <- convertTimeZone t
        UnixTimeNanos (s, ns) :: UnixTimeNanos 'USPacific <- convertTimeZone t
        return $! UnixTimeNanos (s, ns) 

instance Monad m => TimeZoneSeries2 'UTC m 'USPacific where
  convertTimeZone (UnixTimeNanos (s, ns)) = return $! UnixTimeNanos (s-28800, ns)

instance Monad m => TimeZoneSeries2 'UTC m 'USEastern where
  convertTimeZone (UnixTimeNanos (s, ns)) = return $! UnixTimeNanos (s-18000, ns)

instance Monad m => TimeZoneSeries2 tz m tz where
  convertTimeZone = return

convertToUTC
  :: (TimeZoneSeries2 tz m 'UTC)
  => UnixTimeNanos tz -> m (UnixTimeNanos 'UTC)
convertToUTC = convertTimeZone

convertDateTimeZone
  :: (DateTime f, DateTime t)
  => f
  -> IO t
convertDateTimeZone = undefined

