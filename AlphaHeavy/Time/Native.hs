{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module AlphaHeavy.Time.Native where

import Control.Lens
import Foreign (Ptr,nullPtr,alloca,with,peek)
import Foreign.C.Types

import AlphaHeavy.Time.Calendar
import AlphaHeavy.Time.Types
import AlphaHeavy.Time.TM

foreign import ccall time :: Ptr CTime -> IO CTime

getCurrentDateTime :: IO (UnixDateTime 'UTC 'ISO8601)
getCurrentDateTime = alloca (\ x -> time x >>= return . UnixDateTime . fromIntegral . fromEnum)

getCurrentDateTimeNanos :: IO (UnixDateTimeNanos 'UTC 'ISO8601)
getCurrentDateTimeNanos = do
  C'timeval{c'timeval'tv_sec = s, c'timeval'tv_usec = us} <- getTimeOfDay
  return $! UnixDateTimeNanos (fromIntegral s, fromIntegral $ us * 1000)
