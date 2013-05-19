{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AlphaHeavy.Time.Calendar.ISO8601 where

import Control.DeepSeq
import GHC.Generics

import AlphaHeavy.Time.Calendar.Types

data instance Era 'ISO8601
  = BCE
  | CE
    deriving (Eq,Ord,Enum,Show,Read,Generic)

instance NFData (Era 'ISO8601) where
  rnf x = x `seq` ()

data instance DayOfWeek 'ISO8601
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
    deriving (Eq, Ord, Show, Read)

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

data instance MonthOfYear 'ISO8601
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
    deriving (Eq, Ord, Show, Read)

instance NFData (MonthOfYear 'ISO8601) where
  rnf x = x `seq` ()

instance Enum (MonthOfYear 'ISO8601) where
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
