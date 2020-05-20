module World where

import Ball
import Paddle
import Brick

import Foreign.C.Types (CInt)
import SDL
import Control.Monad.IO.Class (MonadIO)
import qualified SDL.Primitive
import Control.Monad.Cont (forM, forM_)

data World = World
  {
    worldLevel :: CInt,
    worldScore :: CInt,
    worldBricks :: [Brick],
    worldPaddle :: Paddle,
    worldBall :: Ball,
    worldStop :: Bool
  }

drawWorld :: (MonadIO m) => World -> Renderer -> m ()
drawWorld w r = do
  let bricks = worldBricks w
  let paddle = worldPaddle w
  let ball = worldBall w

  drawBall ball r
  drawPaddle paddle r
  forM_ bricks $ \b -> drawBrick b r
