module Paddle where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (Left, Right)
import Settings

data Direction = Left
  | Right
  | Up
  | Down
  | Idle

data Paddle = Paddle
  {
    paddlePosition :: V2 CInt,
    paddleSize :: V2 CInt,
    paddleColor :: V4 Word8,
    paddleHidden :: Bool,
    paddleVelocity :: CInt,
    paddleDirection :: Direction
  }

drawPaddle :: (MonadIO m) => Paddle -> Renderer -> m ()
drawPaddle p r = SDL.Primitive.fillRectangle r (paddleSize p) (paddlePosition p) (paddleColor p)

updatePaddle :: Paddle -> CInt ->  Paddle
updatePaddle p mr =
  case paddleDirection p of
    Idle -> p
    Left ->
      Paddle
        (paddlePosition p + getMaxLeft (paddlePosition p) (paddleVelocity p))
        (paddleSize p + getMaxLeft (paddlePosition p) (paddleVelocity p))
        (paddleColor p)
        (paddleHidden p)
        (paddleVelocity p)
        (paddleDirection p)
    Right ->
      Paddle
        (paddlePosition p + getMaxRight (paddlePosition p) (paddleVelocity p))
        (paddleSize p + getMaxRight (paddlePosition p) (paddleVelocity p))
        (paddleColor p)
        (paddleHidden p)
        (paddleVelocity p)
        (paddleDirection p)
    Up -> p
    Down -> p

getMaxLeft :: V2 CInt -> CInt -> V2 CInt
getMaxLeft (V2 x y) v =
  if v <= x
    then V2 (-v) 0
    else V2 0 0

getMaxRight :: V2 CInt -> CInt -> V2 CInt
getMaxRight (V2 x y) v =
  if v + x < Settings.windowWidth - 80 -- paddle size
    then V2 v 0
    else V2 0 0