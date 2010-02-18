{-# LANGUAGE ForeignFunctionInterface #-}

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

import Control.Monad
import Control.Monad.Error

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr

import qualified Data.Text as T

import HSObjC


foreign import ccall "Cocoa.h NSApplicationMain" c_NSApplicationMain :: CInt -> Ptr (Ptr CChar) -> IO CInt

main = c_NSApplicationMain 0 nullPtr


--------------------

{-
    foreign export ccall freeStablePtr :: StablePtr a -> IO ()
    
    freeStablePtr is exported automatically.
-}


{- ### StableId Test ### -}

data StableIdContainer = StableIdContainer {inner :: StableId}

foreign export ccall newStableIdContainer :: Id -> IO (StablePtr StableIdContainer)
newStableIdContainer :: Id -> IO (StablePtr StableIdContainer)
newStableIdContainer obj = do eth <- runErrorT $ fromId obj
                              case eth of 
                                  Left  _ -> return $ castPtrToStablePtr nullPtr
                                  Right x -> newStablePtr $ StableIdContainer x


foreign export ccall retrieveId :: (StablePtr StableIdContainer) -> IO Id
retrieveId :: (StablePtr StableIdContainer) -> IO Id
retrieveId stblId = do x <- deRefStablePtr stblId
                       catchOBJC $ toId (inner x)



{- ### String Test ### -}
foreign export ccall uppercase :: Id -> IO Id
uppercase s = do nsLog "uppercase"
                 x <- toCocoa T.toUpper s

                 catchOBJC $ do des <- fromId 
                                         =<< perfSel0 "description" 
                                         =<< perfSel0 "class" x
                                liftIO $ nsLog $ "Class name: " ++ des
                                return nullPtr

                 return x



{- ### Number Test ### -}
foreign export ccall doubleTest :: Id -> IO Id
doubleTest = toCocoa (id :: (Double -> Double))

foreign export ccall integerTest :: Id -> IO Id
integerTest = toCocoa (id :: (Int -> Int))

{- ### Array Test ### -}
lengthOfStrings :: [String] -> [(Int, String)]
lengthOfStrings = map $ \x -> (length x, x)

foreign export ccall countAllStrings :: Id -> IO Id
countAllStrings = toCocoa lengthOfStrings


{- ### Stored Array Test ### -}

data StoredArray = StoredArray {innerArray :: [(Int, String)]}

foreign export ccall newStoredArray :: Id -> IO (StablePtr StoredArray)
newStoredArray :: Id -> IO (StablePtr StoredArray)
newStoredArray obj = do eth <- runErrorT $ fromId obj
                        case eth of 
                            Left  _ -> return $ castPtrToStablePtr nullPtr
                            Right x -> newStablePtr $ StoredArray $ lengthOfStrings x        

foreign export ccall retrieveStoredArray :: (StablePtr StoredArray) -> IO Id
retrieveStoredArray :: (StablePtr StoredArray) -> IO Id
retrieveStoredArray staId = do x <- deRefStablePtr staId
                               catchOBJC $ toId (innerArray x)
