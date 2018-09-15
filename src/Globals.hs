module Globals
    ( subpixels
    , subvelocity
    , screenWidth
    , screenHeight
    ) where

import Foreign.C.Types (CInt)
    
subpixels :: Integral a => a
subpixels = 32

subvelocity :: Integral a => a
subvelocity = 256

screenWidth :: Integral a => a
screenWidth = 800 * subpixels

screenHeight :: Integral a => a
screenHeight = 600 * subpixels
