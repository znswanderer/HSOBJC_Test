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
import Control.Arrow (first)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr

import Data.Typeable

import qualified Data.Text as T
import qualified Data.Map as M


import HSObjC
import Model

-- The function will be called by the ObjC Controller Proxy
foreign export ccall initController :: Id -> IO Id
foreign export ccall getMethodNames :: Id -> IO Id
foreign export ccall getMethod      :: Id -> Id -> IO Id


-- Start the mainloop!
main = c_NSApplicationMain 0 nullPtr

---

data Controller = Controller { 
                    ctrOutlets :: OutletTable,
                    ctrMethods :: MethodTable,
                    
                    ctrModel   :: Model
                  }

initController :: Id -> IO Id
initController ptr =  catchOBJC $ 
    do outlets <- fromId ptr
       model <- liftIO $ newModel
       let contrl = Controller outlets methods model
       connectOutlets contrl
       toId $ StableValue contrl
 
 
getMethodNames :: Id -> IO Id
getMethodNames contrl = catchOBJC $
    do contrl' <- (return . wrappedValue) =<< fromId contrl
       toId $ M.keys (ctrMethods contrl')

       
getMethod :: Id -> Id -> IO Id       
getMethod contrl mName = catchOBJC $
    do contrl' <- (return . wrappedValue) =<< fromId contrl
       mName' <- fromId mName
       case M.lookup mName' (ctrMethods contrl') of
           Nothing  -> throwError $ "Method " ++ (show mName') ++ " not defined."
           Just act -> toId =<< act contrl'


connectOutlets :: Controller -> IOOBJC ()
connectOutlets contrl = 
    do "haskellTargetButton"       `connect` sayHello
       "string_inputTextField"     `connect` convertString
       "numberSlider"              `connect` presentNumbers
       "stableId_inputTextField"   `connect` storeSimpleName
       "stableId_retrieveButton"   `connect` retrieveSimpleName
       "storeArray_inputTextField" `connect` makeStringAnswer
       "storeArray_retrieveButton" `connect` retrieveStringAnswer

  where 
    outlet :: String -> IOOBJC StableId
    outlet oName = case (T.pack oName) `M.lookup` (ctrOutlets contrl) of
                      Just x  -> return x
                      Nothing -> throwError $ "Could not find outlet named " ++ oName
    
    connect :: (OBJC a, OBJC b) => String -> (a -> IOOBJC b) -> IOOBJC ()
    connect senderName f = outlet senderName >>= makeTarget f 

    {- Target actions for NSControllers
       
       - Cocoa actions never return a value, therefore they return type IOOBJC ().
       - Controller is available via closures.
    -}
    sayHello :: StableId -> IOOBJC ()
    sayHello sender = do nsLog $ "Hello from Controller: " ++ show (ctrOutlets contrl)

    convertString :: StableId -> IOOBJC ()
    convertString sender = 
        do s <- objectValue sender
           nsLog $ "stringValue: " ++ (show s)
           outlet "string_outputTextField" >>= setObjectValue (T.toUpper s)

    presentNumbers :: StableId -> IOOBJC ()
    presentNumbers sender =
        do val <- objectValue sender     -- luckily "objectValue" delivers a NSNumber!
           let x = val :: Double
           let y = (round x) :: Int
           outlet "number_doubleValue" >>= setObjectValue (sqrt x)
           outlet "number_integerValue" >>= setObjectValue (y^2)
           
    storeSimpleName :: StableId -> IOOBJC ()
    storeSimpleName sender =
        do val <- objectValue sender
           liftIO $ (ctrModel contrl) `setSimpleName` val
           -- Enabling/Disabling view elements
           outlet "stableId_retrieveButton" >>= setEnabled True
           setEnabled False sender
           
    retrieveSimpleName :: StableId -> IOOBJC ()
    retrieveSimpleName sender =
        do val <- liftIO $ getSimpleName (ctrModel contrl)
           outlet "stableID_outputTextField" >>= setObjectValue val
           -- Enabling/Disabling view elements
           setEnabled False sender
           outlet "stableId_inputTextField" >>= setEnabled True
           
    makeStringAnswer :: StableId -> IOOBJC ()
    makeStringAnswer sender =
        do val <- objectValue sender
           liftIO $ (ctrModel contrl) `workString` val
           -- Enabling/Disabling view elements
           setEnabled False sender
           outlet "storeArray_retrieveButton" >>= setEnabled True
           
    retrieveStringAnswer :: StableId -> IOOBJC ()
    retrieveStringAnswer sender =
        do val <- liftIO $ stringAnswer (ctrModel contrl)
           outlet "storeArray_stringResults" >>= setObjectValue val
           -- Enabling/Disabling view elements
           setEnabled False sender
           outlet "storeArray_inputTextField" >>= setEnabled True
           
            
-- "Methods"
type MethodTable = M.Map T.Text (Controller -> IOOBJC StableId)

methods :: MethodTable
methods =  M.fromList $ map ( first T.pack )  methodTable
    where     
        methodTable = [ 
                        ("lengthOfStrings", mkMethod lengthOfStrings)
                      ] 

        mkMethod :: (OBJC a) => (Controller -> a) -> (Controller -> IOOBJC StableId)
        mkMethod action = \contrl -> toStblId $ action contrl
        
        

lengthOfStrings :: Controller -> [T.Text] -> IOOBJC [(Int, T.Text)]
lengthOfStrings contrl = return . lengthOfStrings'
    where
        lengthOfStrings' :: [T.Text] -> [(Int, T.Text)]
        lengthOfStrings' = map $ \x -> (T.length x, x)
