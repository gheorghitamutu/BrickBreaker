module Game where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)

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
    paddleColor = V4 0 255 0 100,
    paddleHidden = False
  }

ball :: Ball
ball = Ball
  {
    ballPosition = V2 650 670, -- hellish to convert primitives
    ballRadius = 10,
    ballColor = V4 255 0 0 100,
    ballHidden = False
  }

initialWorld :: World
initialWorld = World
  {
    worldLevel = startingLevel,
    worldScore = startingScore,
    worldBricks = startingBricks,
    worldPaddle = paddle,
    worldBall = ball
  }
