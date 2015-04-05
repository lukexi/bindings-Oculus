{-# LANGUAGE ForeignFunctionInterface #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Bindings.Utils.Windows where

import Foreign.C
import Bindings.OculusRift.Types

getWindowHandle :: String -> IO HWND
getWindowHandle winTitle = withCString winTitle $ \ name -> 
  c_getWindowHandle name 

foreign import ccall unsafe "getWindowHandle" c_getWindowHandle :: CString -> IO HWND 

