module World where

import Ball
import Paddle
import Brick
import Settings

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

updateWorld:: World -> World
updateWorld w = do
  let stop = worldStop w
  let bricks = updateBricks (worldBricks w)
  let paddle = updatePaddle (worldPaddle w) Settings.windowWidth
  let ball = updateBall (worldBall w)

  if stop then w
  else World (worldLevel w) (worldScore w) bricks paddle ball (worldStop w)

drawWorld :: (MonadIO m) => World -> Renderer -> m ()
drawWorld w r = do
  let bricks = worldBricks w
  let paddle = worldPaddle w
  let ball = worldBall w

  drawBall ball r
  drawPaddle paddle r
  forM_ bricks $ \b -> drawBrick b r

changePaddleDirection :: Paddle.Direction -> World -> World
changePaddleDirection d w = do
  let op = worldPaddle w
  let np = Paddle (paddlePosition op) (paddleSize op) (paddleColor op) (paddleHidden op) (paddleVelocity op) d

  World (worldLevel w) (worldScore w) (worldBricks w) np (worldBall w) (worldStop w)
