module Game where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)

windowHeight :: CInt
windowHeight = 720

windowWidth :: CInt
windowWidth = 1280

windowSizes :: V2 CInt
windowSizes = V2 windowWidth windowHeight

startingLevel :: CInt
startingLevel = 0

startingScore :: CInt
startingScore = 0

data Ball = Ball
  {
    ballPosition :: V2 CInt,
    ballRadius :: CInt,
    ballColor :: V4 Word8,
    ballHidden :: Bool
  }

data Paddle = Paddle
  {
    paddlePosition :: V2 CInt,
    paddleSize :: V2 CInt,
    paddleColor :: V4 Word8,
    paddleHidden :: Bool
  }

startingBricks :: [(V4 CInt, Bool)]
startingBricks = [(V4 50 50 50 50, True), (V4 150 150 150 150, True)]

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

data World = World
  {
    worldLevel :: CInt,
    worldScore :: CInt,
    worldBricks :: [(V4 CInt, Bool)],
    worldPaddle :: Paddle,
    worldBall :: Ball
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

getWorldLevel :: World -> CInt
getWorldLevel (World l _ _ _ _) = l

getWorldScore :: World -> CInt
getWorldScore (World _ s _ _ _) = s

getWorldBricks :: World -> [(V4 CInt, Bool)]
getWorldBricks (World _ _ b _ _) = b

getWorldPaddle :: World -> Paddle
getWorldPaddle (World _ _ _ p _) = p

getWorldBall :: World -> Ball
getWorldBall (World _ _ _ _ b) = b

getBallPosition :: Ball -> V2 CInt
getBallPosition (Ball s _ _ _) = s

getBallRadius :: Ball -> CInt
getBallRadius (Ball _ r _ _) = r

getBallColor :: Ball -> V4 Word8
getBallColor (Ball _ _ c _) = c

isBallHidden :: Ball -> Bool
isBallHidden (Ball _ _ _ h) = h

getPaddlePosition :: Paddle -> V2 CInt
getPaddlePosition (Paddle p _ _ _) = p

getPaddleSize:: Paddle -> V2 CInt
getPaddleSize (Paddle _ s _ _) = s

getPaddleColor :: Paddle -> V4 Word8
getPaddleColor (Paddle _ _ c _) = c

isPaddleHidden :: Paddle -> Bool
isPaddleHidden (Paddle _ _ _ h) = h

drawWorld :: (MonadIO m) => World -> Renderer -> m ()
drawWorld w r = do
  let bricks = getWorldBricks w
  let paddle = getWorldPaddle w
  let ball = getWorldBall w

  SDL.Primitive.fillCircle r (getBallPosition ball) (getBallRadius ball) (getBallColor ball)
  SDL.Primitive.fillRectangle r (getPaddleSize paddle) (getPaddlePosition paddle) (getPaddleColor paddle)
