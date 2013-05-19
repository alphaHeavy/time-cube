{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module AlphaHeavy.Time.Types where

import Control.Lens
import Control.DeepSeq
import Data.Int
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import AlphaHeavy.Time.Calendar.Types

data Signed n = Positive n | Negative n

data TimeZone where
  UTC             :: TimeZone
  LocalZone       :: TimeZone
  OffsetZone      :: Signed Nat -> TimeZone
  NamedZone       :: Symbol     -> TimeZone
  UnspecifiedZone :: TimeZone

-- Date/Time Storage
newtype UnixDate          (timeZone :: TimeZone) (cal :: Calendar) = UnixDate          Int64
  deriving (Ord,Eq,NFData,Generic)

_UnixDate :: Iso' (UnixDate tz cal) Int64
_UnixDate = iso (\ (UnixDate x) -> x) UnixDate

instance Typeable (UnixDate tz cal) where
  typeOf _ = mkTyConApp (mkTyCon3 "alphaheavy-time" "AlphaHeavy.Time.Types" "UnixDate") []

newtype UnixTime          (timeZone :: TimeZone) (cal :: Calendar) = UnixTime          Int64
  deriving (Ord,Eq,NFData,Generic)

_UnixTime :: Iso' (UnixTime tz cal) Int64
_UnixTime = iso (\ (UnixTime x) -> x) UnixTime

instance Typeable (UnixTime tz cal) where
  typeOf _ = mkTyConApp (mkTyCon3 "alphaheavy-time" "AlphaHeavy.Time.Types" "UnixTime") []

newtype UnixDateTime      (timeZone :: TimeZone) (cal :: Calendar) = UnixDateTime      Int64
  deriving (Ord,Eq,NFData,Generic)

_UnixDateTime :: Iso' (UnixDateTime tz cal) Int64
_UnixDateTime = iso (\ (UnixDateTime x) -> x) UnixDateTime

instance Typeable (UnixDateTime tz cal) where
  typeOf _ = mkTyConApp (mkTyCon3 "alphaheavy-time" "AlphaHeavy.Time.Types" "UnixDateTime") []

newtype UnixDateTimeNanos (timeZone :: TimeZone) (cal :: Calendar) = UnixDateTimeNanos (Int64, Int32)
  deriving (Ord,Eq,NFData,Generic)

_UnixDateTimeNanos :: Iso' (UnixDateTimeNanos tz cal) (Int64, Int32)
_UnixDateTimeNanos = iso (\ (UnixDateTimeNanos x) -> x) UnixDateTimeNanos

instance Typeable (UnixDateTimeNanos tz cal) where
  typeOf _ = mkTyConApp (mkTyCon3 "alphaheavy-time" "AlphaHeavy.Time.Types" "UnixDateTimeNanos") []

newtype OADateTime        (timeZone :: TimeZone) (cal :: Calendar) = OADateTime        Double
  deriving (Ord,Eq,NFData,Generic)

_OADateTime :: Iso' (OADateTime tz cal) Double
_OADateTime = iso (\ (OADateTime x) -> x) OADateTime

instance Typeable (OADateTime tz cal) where
  typeOf _ = mkTyConApp (mkTyCon3 "alphaheavy-time" "AlphaHeavy.Time.Types" "OADateTime") []

-- data JavaDateTime      (timeZone :: TimeZone) = JavaDateTime      {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- data SystemDateTime    (timeZone :: TimeZone) = SystemDateTime    {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- data FileDateTime      (timeZone :: TimeZone) = FileDateTime      {-# UNPACK #-} !Int {-# UNPACK #-} !Int
-- data GregorianDateTime (timeZone :: TimeZone) = GregorianDateTime !Integer
