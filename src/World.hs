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
    worldBall :: Ball
  }

getWorldLevel :: World -> CInt
getWorldLevel (World l _ _ _ _) = l

getWorldScore :: World -> CInt
getWorldScore (World _ s _ _ _) = s

getWorldBricks :: World -> [Brick]
getWorldBricks (World _ _ b _ _) = b

getWorldPaddle :: World -> Paddle
getWorldPaddle (World _ _ _ p _) = p

getWorldBall :: World -> Ball
getWorldBall (World _ _ _ _ b) = b

drawWorld :: (MonadIO m) => World -> Renderer -> m ()
drawWorld w r = do
  let bricks = getWorldBricks w
  let paddle = getWorldPaddle w
  let ball = getWorldBall w

  drawBall ball r
  drawPaddle paddle r
  forM_ bricks $ \b -> drawBrick b r
