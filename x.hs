{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module X where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Lens as Lens
import Control.Lens.TH
import Data.Int
import Data.Monoid
import Data.Proxy
import Data.Word
import GHC.Generics as G
import GHC.TypeLits
import Unsafe.Coerce

-- Calendars
data Calendar = ISO8601 | Gregorian | Julian | Hijri | Japanese

data Signed n = Positive n | Negative n

data TimeZone where
  UTC             :: TimeZone
  LocalZone       :: TimeZone
  OffsetZone      :: Signed Nat -> TimeZone
  NamedZone       :: Symbol     -> TimeZone
  UnspecifiedZone :: TimeZone

-- Date/Time Storage
newtype UnixDateTime      (timeZone :: TimeZone) = UnixDateTime      Int64
data    UnixDateTimeNanos (timeZone :: TimeZone) = UnixDateTimeNanos {-# UNPACK #-} !Int64 {-# UNPACK #-} !Word32
newtype OADateTime        (timeZone :: TimeZone) = OADateTime        Double

-- data JavaDateTime      (timeZone :: TimeZone) = JavaDateTime      {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- data SystemDateTime    (timeZone :: TimeZone) = SystemDateTime    {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- data FileDateTime      (timeZone :: TimeZone) = FileDateTime      {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- data GregorianDateTime (timeZone :: TimeZone) = GregorianDateTime !Integer

data DateMath (a :: *) (calendar :: Calendar) = DateMath
  { dateMathEra        :: Era        calendar
  , dateMathCentury    :: Century    calendar
  , dateMathYear       :: Year       calendar
  , dateMathMonth      :: Month      calendar
  , dateMathWeek       :: Week       calendar
  , dateMathDay        :: Day        calendar
  , dateMathHour       :: Hour       calendar
  , dateMathMinute     :: Minute     calendar
  , dateMathSecond     :: Second     calendar
  , dateMathNanosecond :: Nanosecond calendar
  }

data DateMath2 (c :: Calendar) (a :: *) = DateMathPure2 a | DateMath2

instance Functor (DateMath2 c) where
  _ `fmap` v@DateMath2{}   = unsafeCoerce v
  f `fmap` DateMathPure2 v = DateMathPure2 $ f v

instance Applicative (DateMath2 c) where
  pure                  = DateMathPure2
  DateMathPure2 f <*> x = f <$> x

data family Era (calendar :: Calendar)

data instance Era 'ISO8601   = BCE | CE
data instance Era 'Gregorian = BC  | AD
data instance Era 'Hijri     = AH
data instance Era 'Japanese  = Meiji | Taisho | Showa | Heisei

data Century     (calendar :: Calendar) = Century     {getCentury :: Int}
data Year        (calendar :: Calendar) = Year        {getYear    :: Int}
data Month       (calendar :: Calendar) = Month       {getMonths  :: Int}
data Week        (calendar :: Calendar) = Week        {getWeeks   :: Int}
data Day         (calendar :: Calendar) = Day         {getDays    :: Int}
data Hour        (calendar :: Calendar) = Hour        {getHours   :: Int}
data Minute      (calendar :: Calendar) = Minute      {getMinutes :: Int}
data Second      (calendar :: Calendar) = Second      {getSeconds :: Int}
data Millisecond (calendar :: Calendar) = Millisecond {getMillis  :: Int}
data Microsecond (calendar :: Calendar) = Microsecond {getMicros  :: Int}
data Nanosecond  (calendar :: Calendar) = Nanosecond  {getNanos   :: Int}
data Picosecond  (calendar :: Calendar) = Picosecond  {getPicos   :: Int}
data Attosecond  (calendar :: Calendar) = Attosecond  {getAttos   :: Int}

class HasDateMath a where
  dateMath :: Simple Iso a (DateMath a (calendar :: Calendar))

instance HasDateMath (UnixDateTime tz) where
  dateMath = error "UnixDateTime/dateMath"

type family DateMathSetter dateTimeType (cal :: Calendar)
type instance DateMathSetter (UnixDateTime tz)      calendar = DateMath (UnixDateTime tz) calendar
type instance DateMathSetter (UnixDateTimeNanos tz) calendar = DateMath (UnixDateTimeNanos tz) calendar
type instance DateMathSetter (OADateTime tz)        calendar = DateMath (OADateTime tz) calendar
type instance DateMathSetter (DateMath x calendar)  calendar = DateMath x calendar

-- when setting a value it needs to be set component-wise so it can eventually be
-- adjusted into canonical form using 'evalDateMath'
type DateGetter a (calendar :: Calendar) s = Getter a (s calendar)
type DateSetter a (calendar :: Calendar) s = Setter (DateMathSetter a calendar) (DateMathSetter a calendar) (s calendar) (s calendar)
type DateLens   a (calendar :: Calendar) s = Lens a (DateMathSetter a calendar) (s calendar) (s calendar)

class HasEra a calendar where
  era :: DateLens a calendar Era

class HasCentury a calendar where
  century :: DateLens a calendar Century

class HasYear a calendar where
  year :: DateLens a calendar Year

class HasMonth a calendar where
  month :: DateLens a calendar Month

class HasMonthOfYear a calendar where
  monthOfYear :: DateLens a calendar MonthOfYear

class HasWeek a calendar where
  week :: DateLens a calendar Week

class HasDay a calendar where
  day :: DateLens a calendar Day

class HasHour a calendar where
  hour :: DateLens a calendar Hour

class HasMinute a calendar where
  minute :: DateLens a calendar Minute

class HasSecond a calendar where
  second :: DateLens a calendar Second

type HasDate a calendar     = (HasYear a calendar, HasMonth  a calendar, HasDay    a calendar)
type HasTime a calendar     = (HasHour a calendar, HasMinute a calendar, HasSecond a calendar)
type HasDateTime a calendar = (HasDate a calendar, HasTime   a calendar)

instance (HasEra a c) => HasEra (DateMath a c) c where
  era = lens g s where
    g = dateMathEra
    s x y = x{dateMathEra = y}

instance (HasCentury a c) => HasCentury (DateMath a c) c where
  century = lens g s where
    g = dateMathCentury
    s x y = x{dateMathCentury = y}

instance (HasYear a c) => HasYear (DateMath a c) c where
  year = lens g s where
    g = dateMathYear
    s x y = x{dateMathYear = y}

instance (HasMonth a c) => HasMonth (DateMath a c) c where
  month = lens g s where
    g = dateMathMonth
    s x y = x{dateMathMonth = y}

instance (HasDay a c) => HasDay (DateMath a c) c where
  day = lens g s where
    g = dateMathDay
    s x y = x{dateMathDay = y}

instance (HasHour a c) => HasHour (DateMath a c) c where
  hour = lens g s where
    g = dateMathHour
    s x y = x{dateMathHour = y}

instance (HasMinute a c) => HasMinute (DateMath a c) c where
  minute = lens g s where
    g = dateMathMinute
    s x y = x{dateMathMinute = y}

instance (HasSecond a c) => HasSecond (DateMath a c) c where
  second = lens g s where
    g = dateMathSecond
    s x y = x{dateMathSecond = y}

instance HasEra (UnixDateTime tz) c where
  era = undefined -- dateMath.era

data family DayOfWeek  (cal :: Calendar) :: *

data instance DayOfWeek 'ISO8601 = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

instance Enum (DayOfWeek 'ISO8601) where
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday
  fromEnum Monday    = 1
  fromEnum Tuesday   = 2
  fromEnum Wednesday = 3
  fromEnum Thursday  = 4
  fromEnum Friday    = 5
  fromEnum Saturday  = 6
  fromEnum Sunday    = 7

{-
instance Enum (DayOfWeek 'Gregorian) where
  toEnum 1 = Sunday
  toEnum 2 = Monday
  toEnum 3 = Tuesday
  toEnum 4 = Wednesday
  toEnum 5 = Thursday
  toEnum 6 = Friday
  toEnum 7 = Saturday
  fromEnum Sunday    = 1
  fromEnum Monday    = 2
  fromEnum Tuesday   = 3
  fromEnum Wednesday = 4
  fromEnum Thursday  = 5
  fromEnum Friday    = 6
  fromEnum Saturday  = 7
-}

data family MonthOfYear (cal :: Calendar) :: *

data instance MonthOfYear 'ISO8601 = January | February | March | April | May | June | July | August | September | October | November | December
data instance MonthOfYear 'Hijri = Muḥarram | Ṣafar | RabīI | RabīII | JumādāI | JumādāII | Rajab | Shabān | Ramaḍān | Shawwāl | DhūalQada | DhūalḤijja

data Duration (xs :: [*])
data Interval

class TimeZoneConversion (timerep :: TimeZone -> *) where
  timeZoneIso :: Simple Iso (timerep tz) (timerep 'UTC)

-- |
-- Convert a time between two timezones via UTC
convertTimeZone :: TimeZoneConversion timerep => Simple Iso (timerep tz0) (timerep tz1)
convertTimeZone = timeZoneIso . Lens.from timeZoneIso

convertIdenticalTimeZone :: forall (timerep :: TimeZone -> *) (tz :: TimeZone) . Simple Iso (timerep tz) (timerep tz)
convertIdenticalTimeZone = id

{-# INLINE[1] convertTimeZone #-}
{-# RULES "convertTimeZone" convertTimeZone = convertIdenticalTimeZone #-}
