module Colours (white, black) where

import SDL.Vect (V4(..))
import GHC.Word

white :: V4 GHC.Word.Word8
white = V4 255 255 255 255

black :: V4 GHC.Word.Word8
black = V4 0 0 0 0



