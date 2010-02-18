{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeSynonymInstances, OverlappingInstances #-}

--  ================================================================
--  Copyright (C) 2010 Tim Scheffler
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--  ================================================================ 

module HSObjC
    (
     Id,
     StableId,
     IOOBJC,
     OBJCError,
     OBJC(..),
     perfSel0,
     perfSel1,
     catchOBJC,
     toCocoa,
     nsLog
    ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)


data ObjcObject
type Id = Ptr ObjcObject    -- typed pointer for all NSObjects 

class OBJC a where
    fromId :: Id -> IOOBJC a
    toId   :: a  -> IOOBJC Id

-- TODO: Own instance of class Error for IOOBJC
type OBJCError = String
type IOOBJC = ErrorT OBJCError IO

foreign import ccall unsafe "Foundation.h NSLog" c_NSLog :: Id -> IO ()

foreign import ccall unsafe "HSObjC_C.h retainId"         c_retainId         :: Id -> IO Id
foreign import ccall unsafe "HSObjC_C.h nsStringToUtf8"   c_nsStringToUtf8   :: Id -> IO CString
foreign import ccall unsafe "HSObjC_C.h utf8ToNSString"   c_utf8ToNSString   :: CString -> IO Id
foreign import ccall unsafe "HSObjC_C.h isNSNumber"       c_isNSNumber       :: Id -> IO CInt
foreign import ccall unsafe "HSObjC_C.h doubleValue"      c_doubleValue      :: Id -> IO CDouble
foreign import ccall unsafe "HSObjC_C.h numberWithDouble" c_numberWithDouble :: CDouble -> IO Id
foreign import ccall unsafe "HSObjC_C.h longValue"        c_longValue        :: Id -> IO CLong
foreign import ccall unsafe "HSObjC_C.h numberWithLong"   c_numberWithLong   :: CLong -> IO Id
foreign import ccall unsafe "HSObjC_C.h arrayWithCArray"  c_arrayWithCArray  :: (Ptr Id) -> CUInt -> IO Id
foreign import ccall unsafe "HSObjC_C.h getObjects"       c_getObjects       :: Id -> IO (Ptr Id)
foreign import ccall unsafe "HSObjC_C.h lengthOfArray"    c_len              :: Id -> IO CUInt

{- These functions are imported "safe", because they might call back into the Haskell
   runtime during execution:
        - An autorelease might trigger a dealloc (not sure about this...), which might free some
          Haskell ForeignPtr.
        - The sending of random methods to random objects surely can trigger callbacks
          to the Haskell runtime.
-}
foreign import ccall        "HSObjC_C.h &releaseId"       c_FunPtr_releaseId :: FunPtr (Id -> IO ())
foreign import ccall        "HSObjC_C.h autoreleaseId"    c_autoreleaseId    :: Id -> IO Id
foreign import ccall        "HSObjC_C.h performMethod0"   c_performMethod0   :: CString -> Id -> IO Id
foreign import ccall        "HSObjC_C.h performMethod1"   c_performMethod1   :: CString -> Id -> Id -> IO Id



-- Some helper functions

checkNullPtr :: String -> IO Id -> IOOBJC Id
checkNullPtr msg act = do ptrId <- liftIO act
                          if ptrId == nullPtr
                             then throwError msg
                             else return ptrId

perfSel0 :: String -> Id -> IOOBJC Id
perfSel0 msg = liftIO . BS.useAsCString (BS8.pack msg) . flip c_performMethod0

perfSel1 :: String -> Id -> Id -> IOOBJC Id
perfSel1 msg obj arg = liftIO $ 
                        BS.useAsCString (BS8.pack msg) $ \cstr -> c_performMethod1 cstr obj arg

catchOBJC :: IOOBJC Id -> IO Id
catchOBJC act = do eth <- runErrorT act
                   case eth of
                      Left err -> do nsLog $ "(Haskell) OBJC error: " ++ err
                                     return nullPtr
                      Right  y -> return y

toCocoa :: (OBJC a, OBJC b) => (a -> b) -> Id -> IO Id
toCocoa f anId = catchOBJC $ toId . f =<< fromId anId

nsLog :: String -> IO ()
nsLog x = do eth <- runErrorT $ toId x
             case eth of
               Left err -> return ()
               Right x  -> c_NSLog x

-- StableId, opaque data type for handling NSObjects
newtype StableId = StableId {
      foreignPtr :: ForeignPtr ObjcObject
    }

instance OBJC StableId where
    toId x = liftIO $ 
             withForeignPtr (foreignPtr x) $ 
                \ptr -> c_retainId ptr >>= c_autoreleaseId
    
    fromId ptr = liftIO $ 
                 do x <- c_retainId ptr >>= newForeignPtr c_FunPtr_releaseId 
                    return $ StableId x
                          

-- NSString handling
instance OBJC BS.ByteString where
    toId x = checkNullPtr "Could not create NSString" $ 
                            BS.useAsCString x c_utf8ToNSString

    fromId x = do ptr <- liftIO $ c_nsStringToUtf8 x
                  if ptr == nullPtr
                    then throwError "not a NSString value"
                    else liftIO $ BS.packCString ptr

instance OBJC T.Text where
    -- via ByteString
    toId = toId . encodeUtf8
    fromId x = return . decodeUtf8 =<< fromId x

instance OBJC String where
    -- via Text
    toId = toId . T.pack
    fromId x = return . T.unpack =<< fromId x


-- NSNumber handling
checkIfNSNumber :: (OBJC a) => (Id -> IO a) -> Id -> IOOBJC a
checkIfNSNumber f ptr = do isNSNumber <- liftIO $ c_isNSNumber ptr
                           case (fromIntegral isNSNumber) of
                             0         -> throwError "not a NSNumber value"
                             otherwise -> liftIO $ f ptr

instance OBJC Double where
    toId = checkNullPtr "Could not create NSNumber" . 
           c_numberWithDouble . realToFrac

    fromId = checkIfNSNumber $ liftM realToFrac . c_doubleValue

instance OBJC Int where
    toId = checkNullPtr "Could not create NSNumber" .
           c_numberWithLong . fromIntegral

    fromId = checkIfNSNumber $ liftM fromIntegral . c_longValue



-- NSArray handling
instance (OBJC a) => OBJC [a] where
    toId xs = mapM toId xs >>= toNSArray'
        where
          toNSArray' :: [Id] -> IOOBJC Id
          toNSArray' x = checkNullPtr "Could not create NSArray" $ 
                              withArrayLen x $ \len ptr -> 
                                  c_arrayWithCArray ptr (fromIntegral len)

    fromId x = fromNSArray' x >>= mapM fromId 
        where
          fromNSArray' :: Id -> IOOBJC [Id]
          fromNSArray' x = do ptr <- liftIO $ c_getObjects x
                              if ptr == nullPtr
                                then throwError "not a NSArray"
                                else liftIO $ do 
                                  len <- c_len x
                                  res <- peekArray (fromIntegral len) ptr
                                  free ptr
                                  return res

-- example for a tuple 
instance (OBJC a, OBJC b) => OBJC (a, b) where
    -- via list and StableId
    toId (a, b) = do -- wrap arguments into opaque StableId, so that we can use them in a list
                     aStId <- fromId =<< toId a :: IOOBJC StableId
                     bStId <- fromId =<< toId b :: IOOBJC StableId
                     toId [aStId, bStId]
                     
    fromId x = do ys <- fromId x :: IOOBJC [StableId]
                  case ys of
                      (aStId:bStId:[]) -> do a <- fromId =<< toId aStId
                                             b <- fromId =<< toId bStId
                                             return (a, b)
                      otherwise        -> throwError "Wrong number of arguments for (,)"

