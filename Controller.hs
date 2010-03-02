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
module Controller
    (
    Controller,
    initController, 
    getMethodNames,
    getMethod
    )
where 

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



---
type OutletTable = M.Map T.Text StableId                     

data Controller = Controller { 
                    ctrOutlets :: OutletTable,
                    ctrMethods :: MethodTable,
                    
                    ctrModel   :: Model
                  }

initController :: Id -> IO Id
initController outletsId = runId $ 
    do outlets <- fromId outletsId
       model <- liftIO $ newModel

       let contrl = Controller outlets methods model

       connectOutlets contrl
       return $ StableValue contrl
 
 
getMethodNames :: Id -> IO Id
getMethodNames contrl = runId $
    do contrl' <- (return . wrappedValue) =<< fromId contrl
       return $ M.keys (ctrMethods contrl')

       
getMethod :: Id -> Id -> IO Id       
getMethod contrl mName = runId $
    do contrl' <- (return . wrappedValue) =<< fromId contrl
       mName' <- fromId mName
       case M.lookup mName' (ctrMethods contrl') of
           Nothing  -> throwError $ "Method " ++ (show mName') ++ " not defined."
           Just act -> act contrl'

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
    {- Target actions for NSControllers
       
       Controller is available via closures.
    -}
    sayHello :: Action
    sayHello sender = 
        do nsLog $ "Hello from Controller: " ++ show (ctrOutlets contrl)

    convertString :: Action
    convertString sender = 
        do s <- sender # objectValue
           nsLog $ "stringValue: " ++ (show s)
           outlet "string_outputTextField" >>= setObjectValue (T.toUpper s)

    presentNumbers :: Action
    presentNumbers sender =
        do val <- sender # objectValue     -- luckily "objectValue" delivers a NSNumber!
           let x = sqrt val       :: Double
           let y = (round val)^2  :: Int
           outlet "number_doubleValue"  >>= setObjectValue x
           outlet "number_integerValue" >>= setObjectValue y
           
    storeSimpleName :: Action
    storeSimpleName sender =
        do val <- sender # objectValue
           model $ setSimpleName val

           outlet "stableId_retrieveButton" >>= setEnabled True
           sender # setEnabled False
           
    retrieveSimpleName :: Action
    retrieveSimpleName sender =
        do val <- model getSimpleName
           outlet "stableID_outputTextField" >>= setObjectValue val

           sender # setEnabled False
           outlet "stableId_inputTextField" >>= setEnabled True
           
    makeStringAnswer :: Action
    makeStringAnswer sender =
        do val <- sender # objectValue
           model $ workString val

           sender # setEnabled False
           outlet "storeArray_retrieveButton" >>= setEnabled True
           
    retrieveStringAnswer :: Action
    retrieveStringAnswer sender =
        do val <- model stringAnswer
           outlet "storeArray_stringResults" >>= setObjectValue val

           sender # setEnabled False
           outlet "storeArray_inputTextField" >>= setEnabled True


    -- ------------------------------------------
    -- Helper
    outlet :: String -> IOOBJC StableId
    outlet oName = case (T.pack oName) `M.lookup` (ctrOutlets contrl) of
                     Just x  -> return x
                     Nothing -> throwError $ "Could not find outlet named " ++ oName

    model :: (Model -> IO a) -> IOOBJC a
    model f = liftIO $ f (ctrModel contrl)

    connect :: String -> Action -> IOOBJC ()
    connect outletName f = outlet outletName >>= makeTarget f 

           
            
-- "Methods"
type MethodTable = M.Map T.Text (Controller -> IOOBJC StableId)

methods :: MethodTable
methods =  M.fromList $ map ( first T.pack )  methodTable
    where     
        methodTable = [ 
                        ("lengthOfStrings", mkMethod lengthOfStrings')
                      ] 

        mkMethod :: (OBJC a) => (Controller -> a) -> (Controller -> IOOBJC StableId)
        mkMethod action = \contrl -> toStableId $ action contrl
        
        lengthOfStrings' contrl = (return :: a -> IOOBJC a) . lengthOfStrings 
