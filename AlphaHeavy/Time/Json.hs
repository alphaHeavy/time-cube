{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module AlphaHeavy.Time.Json where

import Control.Applicative
import Data.Aeson
import AlphaHeavy.Time

instance ToJSON (UnixDateTimeNanos 'UTC 'ISO8601) where
  toJSON (UnixDateTimeNanos (dtm, ns))
    | ns < 1000000000 = toJSON (dtm * 1000 + fromIntegral ns `div` 1000000) -- reduce to milliseconds resolution
    | otherwise      = error "over 1 second of nanoseconds"

instance ToJSON (UnixDateTime 'UTC 'ISO8601) where
  toJSON (UnixDateTime dtm) = toJSON (dtm * 1000)

instance ToJSON (UnixDate 'UTC 'ISO8601) where
  toJSON (UnixDate dt) = toJSON (dt * 1000)

instance FromJSON (UnixDateTimeNanos 'UTC 'ISO8601) where
  parseJSON val = do
    (dtm, ms) <- (\v -> v `divMod` 1000) <$> parseJSON val
    return $! UnixDateTimeNanos (dtm, fromIntegral $ ms * 100000)

instance FromJSON (UnixDateTime 'UTC 'ISO8601) where
  parseJSON val = UnixDateTime <$> dtm
    where dtm = (\v -> v `div` 1000) <$> parseJSON val

instance FromJSON (UnixDate 'UTC 'ISO8601) where
  parseJSON val = UnixDate <$> dt
    where dt = (\v -> 86400 * (v `div` (1000 * 86400))) <$> parseJSON val
