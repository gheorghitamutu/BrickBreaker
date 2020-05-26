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
import Prelude hiding (LT)

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
  let ballHitPaddle = switchBallDirectionWhenItHitsPaddle (worldBall w) paddle
  let ballHitBricks =
        if ballCollidedBrick (worldBall w) bricks
          then switchBallDirectionWhenItHitsBrick ballHitPaddle (getCollidedBrick (worldBall w) bricks)
          else ballHitPaddle
  let ball = updateBall ballHitBricks
  let bricksNotHit = getNotCollidedBricks ballHitPaddle bricks
  
  if stop
    then w
    else World (worldLevel w) (worldScore w) bricksNotHit paddle ball (null bricksNotHit)

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

aabbCollision :: V2 CInt -> V2 CInt -> V2 CInt -> V2 CInt -> Bool
aabbCollision (V2 al au) (V2 ar ad) (V2 bl bu) (V2 br bd) = ar >= bl && al <= br && ad >= bu && au <= bd

ballBrickAabbCollision :: Ball -> Brick -> Bool
ballBrickAabbCollision b br = do
  let bp = ballPosition b
  let rb = ballRadius b
  let ltb = bp - V2 rb rb
  let rdb = bp + V2 rb rb

  let ltbr = brickPosition br
  let rdbr = brickSize br

  aabbCollision ltb rdb ltbr rdbr

switchBallDirectionWhenItHitsPaddle :: Ball -> Paddle -> Ball
switchBallDirectionWhenItHitsPaddle b p = do
  let bp = ballPosition b
  let rb = ballRadius b
  let ltb = bp - V2 rb rb
  let rdb = bp + V2 rb rb
  let db = ballDirection b

  let ltp = paddlePosition p
  let rdp = paddleSize p

  if aabbCollision ltb rdb ltp rdp
    then case db of
           LD -> Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) LT
           RD -> Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) RT
           LT -> b
           RT -> b
    else b

ballCollidedBrick :: Ball -> [Brick] -> Bool
ballCollidedBrick b bs = do
  let bricksCollided = filter (ballBrickAabbCollision b) bs
  not (null bricksCollided)

getCollidedBrick :: Ball -> [Brick] -> Brick
getCollidedBrick b bs = do
  let bricksCollided = filter (ballBrickAabbCollision b) bs
  head bricksCollided

getNotCollidedBricks :: Ball -> [Brick] -> [Brick]
getNotCollidedBricks b = filter (not . ballBrickAabbCollision b)

switchBallDirectionWhenItHitsBrick :: Ball -> Brick -> Ball
switchBallDirectionWhenItHitsBrick b br = do
  let bp = ballPosition b
  let rb = ballRadius b
  let ltb = bp - V2 rb rb
  let rdb = bp + V2 rb rb
  let db = ballDirection b

  let ltbr = brickPosition br
  let rdbr = brickSize br

  case db of
     LD -> Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) LT
     RD -> Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) RT
     LT -> Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) LD
     RT -> Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) RD