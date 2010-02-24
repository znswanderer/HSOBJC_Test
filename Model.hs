{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

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

import Data.Typeable

import qualified Data.Text as T
import qualified Data.Map as M


import HSObjC


foreign import ccall "Cocoa.h NSApplicationMain" c_NSApplicationMain :: CInt -> Ptr (Ptr CChar) -> IO CInt

main = c_NSApplicationMain 0 nullPtr


--------------------

{-
    foreign export ccall freeStablePtr :: StablePtr a -> IO ()
    
    freeStablePtr is exported automatically.
-}


foreign export ccall getFunctionList :: IO Id
getFunctionList :: IO Id
getFunctionList = catchOBJC $ 
                    do funcList' <- forM funcList $ \(key, funcAction) -> 
                                            do func <- funcAction
                                               return (T.pack key, func)
                       toId $ M.fromList funcList'


wrapIO :: (OBJC a) => a -> IOOBJC StableId
wrapIO f = fromId =<< toId f

wrap1 :: (OBJC a, OBJC b) => (a -> b) -> IOOBJC StableId
wrap1 f = wrapIO1 (return . f)
    where wrapIO1 :: (OBJC a, OBJC b) => (a -> IO b) -> IOOBJC StableId
          wrapIO1 f = fromId =<< toId f
    
wrap2 :: (OBJC a, OBJC b, OBJC c) => (a -> b -> c) -> IOOBJC StableId
wrap2 f = wrapIO2 $ \x y -> return $ f x y
  where wrapIO2 :: (OBJC a, OBJC b, OBJC c) => (a -> b -> IO c) -> IOOBJC StableId
        wrapIO2 f = fromId =<< toId f
                   

-- we have to wrap in order to get an array with strict types
funcList = [ ("squareInt",       wrap1  ( (\x -> x ^ 2) :: (Int -> Int) )       )
           , ("doubleSqrt",      wrap1  ( sqrt          :: (Double -> Double) ) )
           , ("uppercase2",      wrapIO uppercase2                              )
           , ("lengthOfStrings", wrap1  lengthOfStrings                         )
           ]




uppercase2 :: T.Text -> IO T.Text
uppercase2 s = do nsLog "uppercase2"
                  x <- catchOBJC $ toId $ T.toUpper s

                  catchOBJC $ do des <- fromId 
                                           =<< perfSel0 "description" 
                                           =<< perfSel0 "class" x
                                 liftIO $ nsLog $ "Class name: " ++ (T.unpack des)
                                 return nullPtr

                  return $ T.toUpper s

{- ### StableId Test ### -}

data StableIdContainer = StableIdContainer {inner :: StableId}
    deriving (Typeable)

foreign export ccall newStableIdContainer :: Id -> IO (StablePtr StableIdContainer)
newStableIdContainer :: Id -> IO (StablePtr StableIdContainer)
newStableIdContainer obj = do eth <- runErrorT $ fromId obj
                              case eth of 
                                  Left  _ -> return $ castPtrToStablePtr nullPtr
                                  Right x -> do newStablePtr $ StableIdContainer x


foreign export ccall retrieveId :: (StablePtr StableIdContainer) -> IO Id
retrieveId :: (StablePtr StableIdContainer) -> IO Id
retrieveId stblId = do x <- deRefStablePtr stblId
                       catchOBJC $ toId (inner x)






{- ### Array Test ### -}
lengthOfStrings :: [T.Text] -> [(Int, T.Text)]
lengthOfStrings = map $ \x -> (T.length x, x)


{- ### Stored Array Test ### -}

-- data StoredArray = StoredArray {innerArray :: [(Int, String)]}

foreign export ccall newStoredArray :: Id -> IO Id
newStoredArray = toCocoa $ StableValue . lengthOfStrings 

{-
newStoredArray :: Id -> IO (StablePtr StoredArray)
newStoredArray obj = do eth <- runErrorT $ fromId obj
                        case eth of 
                            Left  _ -> return $ castPtrToStablePtr nullPtr
                            Right x -> StableValue $ lengthOfStrings x        
-}

foreign export ccall retrieveStoredArray :: Id -> IO Id
retrieveStoredArray = toCocoa $ \x-> (wrappedValue x :: [(Int, T.Text)])

{-
retrieveStoredArray :: (StablePtr StoredArray) -> IO Id
retrieveStoredArray staId = do x <- deRefStablePtr staId
                               catchOBJC $ toId (innerArray x)
-}

