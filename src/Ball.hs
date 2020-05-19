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

getBallPosition :: Ball -> V2 CInt
getBallPosition (Ball s _ _ _) = s

getBallRadius :: Ball -> CInt
getBallRadius (Ball _ r _ _) = r

getBallColor :: Ball -> V4 Word8
getBallColor (Ball _ _ c _) = c

isBallHidden :: Ball -> Bool
isBallHidden (Ball _ _ _ h) = h

drawBall :: (MonadIO m) => Ball -> Renderer -> m ()
drawBall b r = SDL.Primitive.fillCircle r (getBallPosition b) (getBallRadius b) (getBallColor b)