module Settings where

import Foreign.C.Types (CInt)
import SDL

windowHeight :: CInt
windowHeight = 720

windowWidth :: CInt
windowWidth = 1280

windowSizes :: V2 CInt
windowSizes = V2 windowWidth windowHeight
