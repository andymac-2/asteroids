module Colors
    ( Color
    , white
    , black
    ) where

import SDL
import Data.Word (Word8)

type Color = V4 Word8

black :: Color
black = V4 0 0 0 255

white :: Color
white = V4 255 255 255 255
