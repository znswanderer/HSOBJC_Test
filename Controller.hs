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

import Data.Typeable

import qualified Data.Text as T
import qualified Data.Map as M


import HSObjC
import Model

-- The function will be called by the ObjC Controller Proxy
foreign export ccall initController :: Id -> IO Id


-- Start the mainloop!
main = c_NSApplicationMain 0 nullPtr

---

data Controller = Controller { 
                    ctrOutlets :: OutletTable
                  } deriving (Show)

initController :: Id -> IO Id
initController ptr =  catchOBJC $ 
    do outlets <- fromId ptr
       let contrl = Controller outlets
       
       contrl `connectOutlets` outlets
       toId $ StableValue contrl
                           

connectOutlets :: Controller -> OutletTable -> IOOBJC ()
connectOutlets contrl outlets = 
    do connect "haskellTargetButton" sayHello
       connect "string_inputTextField" convertString
       connect "numberSlider" presentNumbers

  where 
    outlet :: (OBJC a) => String -> (StableId -> IOOBJC a) -> IOOBJC a
    outlet oName f = withOutlet outlets oName f

    connect :: (OBJC a, OBJC b) => String -> (a -> IOOBJC b) -> IOOBJC ()
    connect senderName f = outlet senderName $ \x -> makeTarget f x

    setObjectValue :: (OBJC a) =>  a -> StableId -> IOOBJC ()
    setObjectValue val target = perfSel1' target "setObjectValue:" val
    
    {- Target actions for NSControllers
       
       - Cocoa actions never return a value, therefore they return type IOOBJC ().
       - The controller and outlets are available via closures.
    -}
    sayHello :: StableId -> IOOBJC ()
    sayHello sender = do nsLog $ "Hello from Controller: " ++ show contrl

    convertString :: StableId -> IOOBJC ()
    convertString sender = 
        do s <- sender `perfSel0` "stringValue"
           nsLog $ "stringValue: " ++ (show s)
           outlet "string_outputTextField" $ setObjectValue (T.toUpper s)

    presentNumbers :: StableId -> IOOBJC ()
    presentNumbers sender =
        do val <- sender `perfSel0` "objectValue"     -- luckily "objectValue" delivers a NSNumber!
           let x = val :: Double
           let y = (round x) :: Int
           outlet "number_doubleValue" $ setObjectValue (sqrt x)
           outlet "number_integerValue" $ setObjectValue (y^2)
           
