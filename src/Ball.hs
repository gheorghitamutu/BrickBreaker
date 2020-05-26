module Ball where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (Left, Right)

data Direction = Left
  | Right
  | Up
  | Down
  | Idle

data Ball = Ball
  {
    ballPosition :: V2 CInt,
    ballRadius :: CInt,
    ballColor :: V4 Word8,
    ballHidden :: Bool,
    ballVelocity :: CInt,
    ballDirection :: Direction
  }

drawBall :: (MonadIO m) => Ball -> Renderer -> m ()
drawBall b r = SDL.Primitive.fillCircle r (ballPosition b) (ballRadius b) (ballColor b)

updateBall :: Ball -> Ball
updateBall b =
  case ballDirection b of
    Idle -> b
    Left -> b
    Right -> b
    Up -> b
    Down -> b