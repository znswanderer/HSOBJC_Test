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

import Foreign
import HSObjC
-- I need the following imports to trigger inclusion of these modules
-- in the final build process!?
import Controller
import Model


-- Start the mainloop!
main = c_NSApplicationMain 0 nullPtr
