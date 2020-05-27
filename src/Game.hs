module Game (loop, initialWorld) where

import SDL
import qualified SDL.Primitive
import qualified SDL.Font
import qualified Data.Text
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cont (unless, when)
import SDL.Framerate
import EventHandler (eventToIntent, shouldQuit, actionHandler, shouldRestart)

import Ball
import Paddle
import Brick
import World
import Settings

startingLevel :: CInt
startingLevel = 0

startingScore :: Int
startingScore = 0

startingBricks :: [Brick]
startingBricks = generateBricks

paddle :: Paddle
paddle = Paddle
  {
    paddlePosition = V2 600 680,
    paddleSize = V2 700 700,
    paddleColor = V4 255 165 0 255,
    paddleHidden = False,
    paddleVelocity = 7,
    paddleDirection = Paddle.Idle
  }

ball :: Ball
ball = Ball
  {
    ballPosition = V2 650 670, -- hellish to convert primitives
    ballRadius = 20,
    ballColor = V4 0 191 255 255,
    ballHidden = False,
    ballVelocity = 5,
    ballDirection = Ball.LT
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
      let restart = shouldRestart action

      let stateUpdatedWorld = actionHandler action w
      let updatedWorld = if not restart then updateWorld stateUpdatedWorld else initialWorld

      draw r updatedWorld


      SDL.Framerate.delay_ fpsm

      unless quit (loop r updatedWorld fpsm)