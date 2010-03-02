
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

module Model
    (
    Model, newModel,
    getSimpleName, setSimpleName,
    Answer, workString, stringAnswer,
    lengthOfStrings
    )
where 

import Control.Monad
import Control.Monad.Error
import Data.IORef
import qualified Data.Text as T


type Answer = [(Int, T.Text)]

data Model = Model {
                mdSimpleName    :: IORef T.Text,
                mdStringsLength :: IORef Answer
             }

newModel :: IO Model
newModel = do text <- newIORef $ T.pack ""
              stringsLength <- newIORef $ []
              return $ Model text stringsLength
               
getSimpleName :: Model -> IO T.Text
getSimpleName mdl = readIORef (mdSimpleName mdl)

setSimpleName :: T.Text -> Model -> IO ()
setSimpleName text mdl = writeIORef (mdSimpleName mdl) text

workString :: T.Text -> Model -> IO ()
workString input mdl = writeIORef (mdStringsLength mdl) (lengthOfStrings parts) 
    where
        parts = T.split (T.pack ", ") input
        

stringAnswer :: Model -> IO Answer
stringAnswer mdl = readIORef (mdStringsLength mdl)


lengthOfStrings :: [T.Text] -> Answer
lengthOfStrings = map $ \x -> (T.length x, x)

