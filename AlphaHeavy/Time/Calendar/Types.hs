{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module AlphaHeavy.Time.Calendar.Types where

import Control.DeepSeq
import Data.Int
import GHC.Generics

data Calendar = ISO8601 | Gregorian | Julian | Hijri | Japanese
  deriving (Eq,Ord,Enum,Show,Read,Generic)

instance NFData Calendar where
  rnf x = x `seq` ()

data family Era (cal :: Calendar)

newtype Century (cal :: Calendar) = Century {getCentury :: Int32}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Year    (cal :: Calendar) = Year    {getYears   :: Int32}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Month   (cal :: Calendar) = Month   {getMonths  :: Int32}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Week    (cal :: Calendar) = Week    {getWeeks   :: Int32}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Day     (cal :: Calendar) = Day     {getDays    :: Int32}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Hour    (cal :: Calendar) = Hour    {getHours   :: Int32}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Minute  (cal :: Calendar) = Minute  {getMinutes :: Int32}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Second  (cal :: Calendar) = Second  {getSeconds :: Int64}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Millis  (cal :: Calendar) = Millis  {getMillis  :: Int64}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Micros  (cal :: Calendar) = Micros  {getMicros  :: Int64}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Nanos   (cal :: Calendar) = Nanos   {getNanos   :: Int64}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Picos   (cal :: Calendar) = Picos   {getPicos   :: Int64}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

newtype Attos   (cal :: Calendar) = Attos   {getAttos   :: Int64}
  deriving (Eq,Ord,Num,Real,Integral,Enum,Show,Read,NFData,Generic)

data family DayOfWeek  (cal :: Calendar) :: *
data family MonthOfYear (cal :: Calendar) :: *

data Duration (xs :: [*])
data Interval
