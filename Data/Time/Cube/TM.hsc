{-# LANGUAGE MultiParamTypeClasses #-}

#include <bindings.dsl.h>
#include <time.h>
#include <sys/time.h>

module Data.Time.Cube.TM where

import Data.Convertible
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Marshal.Utils (with)
import Foreign.Storable

#starttype struct tm
#field tm_sec    , CInt
#field tm_min    , CInt
#field tm_hour   , CInt
#field tm_mday   , CInt
#field tm_mon    , CInt
#field tm_year   , CInt
#field tm_wday   , CInt
#field tm_yday   , CInt
#field tm_isdst  , CInt
#field tm_gmtoff , CLong
#field tm_zone   , CString
#stoptype

#starttype struct timespec
#field tv_sec    , CLong
#field tv_nsec   , CLong
#stoptype

#starttype struct timeval
#field tv_sec    , CLong
#field tv_usec   , CLong
#stoptype

#starttype struct timezone
#field tz_minuteswest   , CInt
#field tz_dsttime       , CInt
#stoptype

#num CLOCKS_PER_SEC

#ccall asctime_r , Ptr <tm> -> CString -> IO CString
#ccall ctime_r , Ptr CTime -> Ptr <tm> -> IO CString
#ccall gmtime_r , Ptr CTime -> Ptr <tm> -> IO (Ptr <tm>)
#ccall localtime_r , Ptr CTime -> Ptr <tm> -> IO (Ptr <tm>)
#ccall timegm , Ptr <tm> -> IO CTime
#ccall mktime , Ptr <tm> -> IO CTime
#ccall gettimeofday , Ptr <timeval> -> Ptr <timezone> -> IO CInt

instance Convertible CTime C'tm where
  safeConvert ctime = Right (unsafeLocalState conv)
    where conv = with ctime (\ ctime' -> alloca (\ tm -> c'gmtime_r ctime' tm >>= peek))

instance Convertible C'tm CTime where
  safeConvert tm = Right (unsafeLocalState conv)
    where conv = with tm (\tm_ptr -> c'timegm tm_ptr)

withField :: CTime -> (C'tm -> C'tm) -> CTime
withField ct setf = let tm = convert ct :: C'tm
               in convert $ setf tm

getTimeOfDay :: IO C'timeval
getTimeOfDay = alloca (\tmptr -> c'gettimeofday tmptr nullPtr >>= getResult tmptr)
  where getResult tmptr 0 = peek tmptr
        getResult _ err = error $ "Error in call to gettimeofday: " ++ show err
