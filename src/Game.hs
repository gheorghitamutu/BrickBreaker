module Game (loop, initialWorld) where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cont (unless)
import SDL.Framerate
import EventHandler (eventToIntent, shouldQuit, actionHandler)

import Ball
import Paddle
import Brick
import World
import Settings

startingLevel :: CInt
startingLevel = 0

startingScore :: CInt
startingScore = 0

startingBricks :: [Brick]
startingBricks = [
  Brick
    {
      brickPosition = V2 50 100,
      brickSize = V2 150 150,
      brickColor = V4 0 128 0 100,
      brickHidden = False
    },
  Brick
    {
      brickPosition = V2 200 100,
      brickSize = V2 300 150,
      brickColor = V4 0 128 0 100,
      brickHidden = False
    },
  Brick
    {
      brickPosition = V2 350 100,
      brickSize = V2 450 150,
      brickColor = V4 0 128 0 100,
      brickHidden = False
    }]

paddle :: Paddle
paddle = Paddle
  {
    paddlePosition = V2 600 680,
    paddleSize = V2 700 700,
    paddleColor = V4 255 187 80 100,
    paddleHidden = False,
    paddleVelocity = 5,
    paddleDirection = Paddle.Idle
  }

ball :: Ball
ball = Ball
  {
    ballPosition = V2 650 670, -- hellish to convert primitives
    ballRadius = 10,
    ballColor = V4 255 0 0 100,
    ballHidden = False,
    ballVelocity = 1,
    ballDirection = Ball.Idle
  }

initialWorld :: World
initialWorld = World
  {
    worldLevel = startingLevel,
    worldScore = startingScore,
    worldBricks = startingBricks,
    worldPaddle = paddle,
    worldBall = ball,
    worldStop = False
  }

draw :: SDL.Renderer -> World -> IO ()
draw r w = do
  clearScreen r
  drawWorld w r
  SDL.present r

clearScreen :: (MonadIO m) => SDL.Renderer -> m ()
clearScreen r = do
  SDL.rendererDrawColor r $= SDL.V4 255 255 255 100
  SDL.clear r



loop :: SDL.Renderer -> World -> SDL.Framerate.Manager -> IO ()
loop r w fpsm = do
      event <- SDL.pollEvent
      let action = eventToIntent event
      let quit = shouldQuit action

      let newWorld = actionHandler action w
      let updatedWorld = updateWorld newWorld
      draw r updatedWorld

      SDL.Framerate.delay_ fpsm

      unless quit (loop r updatedWorld fpsm)