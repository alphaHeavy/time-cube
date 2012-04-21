{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
module Data.Time.Cube.Lens
  ( DateTimeLensT(..)

  -- * Raw date components
  , datePart

  -- * Lens accessors
  -- ** Pure
  , Data.Time.Cube.Lens.get
  , Data.Time.Cube.Lens.set
  , Data.Time.Cube.Lens.modify

  -- ** Monadic (currently not very monadic...)
  , Data.Time.Cube.Lens.getM
  , Data.Time.Cube.Lens.getM2
  , Data.Time.Cube.Lens.setM
  , Data.Time.Cube.Lens.modifyM

  -- * Helper lenses
  -- , epoch
  -- , era
  -- , century
  , year
  , month
  , monthOfYear
  , week
  , day
  , dayOfWeek
  , hour
  , minute
  , second
  , milli
  , nano
  , pico

  , timeZoneOffset
  ) where

import Data.Time.Cube.Types
import Control.Arrow (Kleisli(..), runKleisli, arr)
import Control.Category
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy as State
import Data.Label.Abstract as A
import qualified Data.Time as HT
import Prelude hiding ((.), id)

dateTimeLens
  :: (f -> StateT c m a)
  -> (a -> f -> StateT c m f)
  -> DateTimeLensT m c f a
dateTimeLens g s = DateTimeLensT (A.lens (Kleisli g) (Kleisli (uncurry s)))

-- |
-- A lens from a time into a time component
datePart :: (Functor m, Monad m, DateTimePart c f a) => DateTimeLensT m c f a
datePart = dateTimeLens dtg dts

newtype DateTimeLensT m c f a = DateTimeLensT{unDateTimeLens :: A.Lens (Kleisli (StateT c m)) f a}

deriving instance Monad m => Category (DateTimeLensT m c)

runDateTimeLensT
  :: (Monad m, DateTime f)
  => Kleisli (StateT (DateTimeComponents f) m) f a
  -> f
  -> m a
runDateTimeLensT l f = evalStateT (runKleisli l f) (unpack f)

getM
  :: (Functor m, Monad m, DateTime f)
  => DateTimeLensT m (DateTimeComponents f) f a
  -> f
  -> m a
getM = runDateTimeLensT . A.get . unDateTimeLens

getM2
  :: (Functor m, Monad m, DateTime f)
  => (DateTimeLensT m (DateTimeComponents f) f a, DateTimeLensT m (DateTimeComponents f) f b)
  -> f
  -> m (a, b)
getM2 (l1, l2) f = do
  a <- runDateTimeLensT (A.get (unDateTimeLens l1)) f
  b <- runDateTimeLensT (A.get (unDateTimeLens l2)) f
  return (a, b)

setM
  :: (Functor m, Monad m, DateTime f)
  => DateTimeLensT m (DateTimeComponents f) f a
  -> a
  -> f
  -> m f
setM (DateTimeLensT l) v = runDateTimeLensT (A.set l . arr (v,))

modifyM
  :: (Functor m, Monad m, DateTime f)
  => DateTimeLensT m (DateTimeComponents f) f a
  -> (a -> a)
  -> f
  -> m f
modifyM (DateTimeLensT l) v = runDateTimeLensT (A.modify l . arr (arr v,))

get
  :: DateTime f
  => DateTimeLensT Identity (DateTimeComponents f) f a
  -> f
  -> a
get l = runIdentity . getM l

set
  :: DateTime f
  => DateTimeLensT Identity (DateTimeComponents f) f a
  -> a
  -> f
  -> f
set l v = runIdentity . setM l v

modify
  :: DateTime f
  => DateTimeLensT Identity (DateTimeComponents f) f a
  -> (a -> a)
  -> f
  -> f
modify l v = runIdentity . modifyM l v

year
  :: (Functor m, Monad m, DateTimePart c f Year)
  => DateTimeLensT m c f Year
year = datePart

month
  :: (Functor m, Monad m, DateTimePart c f Month)
  => DateTimeLensT m c f Month
month = datePart

monthOfYear
  :: (Functor m, Monad m, DateTimePart c f MonthOfYear)
  => DateTimeLensT m c f MonthOfYear
monthOfYear = datePart

week
  :: (Functor m, Monad m, DateTimePart c f Week)
  => DateTimeLensT m c f Week
week = datePart

dayOfWeek
  :: (Functor m, Monad m, DateTimePart c f DayOfWeek)
  => DateTimeLensT m c f DayOfWeek
dayOfWeek = datePart

day
  :: (Functor m, Monad m, DateTimePart c f Day)
  => DateTimeLensT m c f Day
day = datePart

hour
  :: (Functor m, Monad m, DateTimePart c f Hour)
  => DateTimeLensT m c f Hour
hour = datePart

minute
  :: (Functor m, Monad m, DateTimePart c f Minute)
  => DateTimeLensT m c f Minute
minute = datePart

second
  :: (Functor m, Monad m, DateTimePart c f Second)
  => DateTimeLensT m c f Second
second = datePart

milli
  :: (Functor m, Monad m, DateTimePart c f Milli)
  => DateTimeLensT m c f Milli
milli = datePart

nano
  :: (Functor m, Monad m, DateTimePart c f Nano)
  => DateTimeLensT m c f Nano
nano = datePart

pico
  :: (Monad m, Functor m, DateTimePart c f Pico)
  => DateTimeLensT m c f Pico
pico = datePart

timeZoneOffset
  :: (Functor m, Monad m, DateTimePart c f TimeZoneOffset)
  => DateTimeLensT m c f TimeZoneOffset
timeZoneOffset = datePart

