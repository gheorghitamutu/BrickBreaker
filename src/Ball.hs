module Ball where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)

data Ball = Ball
  {
    ballPosition :: V2 CInt,
    ballRadius :: CInt,
    ballColor :: V4 Word8,
    ballHidden :: Bool
  }

drawBall :: (MonadIO m) => Ball -> Renderer -> m ()
drawBall b r = SDL.Primitive.fillCircle r (ballPosition b) (ballRadius b) (ballColor b)