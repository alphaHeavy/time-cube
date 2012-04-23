{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
module Data.Time.Cube.Types
  ( DateTime(..)
  , DateTimePart(..)
  , TimeZone(..)
  , TimeZoneOffset(..)

  -- * Time conversions
  , convertDateTime

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

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Lazy as State
import Data.Int
import Data.Data
import Prelude hiding ((.), id)

-- |
-- Compute relative times since the epoch
data Epoch

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

data TimeZone = UTC | LocalTime | USEastern | USPacific -- | forall a . TimeZone a
  deriving (Show, Eq, Ord)

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


-- |
-- Functions used to build lenses
class DateTimePart c f a where
  dtg :: (Functor m, Monad m) => f -> StateT c m a
  dts :: (Functor m, Monad m) => a -> f -> StateT c m f

convertDateTime
  :: (DateTime f, DateTime t, DateTimeZone f ~ DateTimeZone t)
  => f
  -> Maybe t
convertDateTime = undefined

class Duration (a :: *)

instance Duration Year
instance Duration Month
instance Duration Week
instance Duration Day
instance Duration Hour
instance Duration Minute
instance Duration Second
instance Duration Milli
instance Duration Nano
instance Duration Pico

