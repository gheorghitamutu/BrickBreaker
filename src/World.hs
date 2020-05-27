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

import System.Random (StdGen, mkStdGen, next, randomIO, randomRIO)
import GHC.IO.Unsafe (unsafePerformIO)
import Data.IORef.Extra (IORef, writeIORef, readIORef, newIORef)

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


switchBallDirectionWhenItHitsPaddle :: Ball -> Paddle -> Ball
switchBallDirectionWhenItHitsPaddle b p = do
  let bp = ballPosition b
  let rb = ballRadius b
  let ltb = bp - V2 rb rb
  let rdb = bp + V2 rb rb
  let db = ballDirection b

  let ltp = paddlePosition p
  let rdp = paddleSize p
  let dp = paddleDirection p

  if aabbCollision ltb rdb ltp rdp
    then case db of
           LD ->
            case dp of
              Paddle.Left ->
                Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) LT
              Paddle.Right ->
                Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) RT
           RD ->
            case dp of
              Paddle.Right ->
                Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) RT
              Paddle.Left ->
                Ball bp rb (ballColor b) (ballHidden b) (ballVelocity b) LT
           LT -> b
           RT -> b
    else b

ballBrickAabbCollision :: Ball -> Brick -> Bool
ballBrickAabbCollision b br = do
  let bp = ballPosition b
  let rb = ballRadius b
  let ltb = bp - V2 rb rb
  let rdb = bp + V2 rb rb

  let ltbr = brickPosition br
  let rdbr = brickSize br

  aabbCollision ltb rdb ltbr rdbr

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

rawBricksLT :: CInt -> CInt -> IO[CInt]
rawBricksLT 0 maxRD = return []
rawBricksLT n maxRD = do
  r <- randomRIO (0, maxRD)
  rs <- rawBricksLT (n-1) maxRD
  return (r:rs)

rawBricksLTFilter :: CInt -> CInt -> Bool
rawBricksLTFilter lt1 lt2 = do -- hardcoded brick size...
  let bp1 = V2 lt1 (lt1 + 50)
  let bs1 = V2 (lt1 + 100) (lt1 + 100)

  let bp2 = V2 lt2 (lt2 + 50)
  let bs2 = V2 (lt2 + 100) (lt2 + 100)

  aabbCollision bp1 bs1 bp2 bs2

brickFromLT :: CInt -> CInt -> Brick
brickFromLT x y = Brick (V2 x y) (V2 (x + 100) (y + 50)) (V4 0 128 0 100) False

mapBricks :: [CInt] -> [CInt] -> [Brick]
mapBricks = zipWith brickFromLT

-- https://gist.github.com/ppetr/3693348
coerce :: IO[CInt] -> [CInt] -- a -> b
coerce x = unsafePerformIO $ do
    writeIORef test [x]
    [y] <- readIORef test
    return y
  where
    test :: IORef [a]
    test = unsafePerformIO $ newIORef []

generateBricks :: [Brick]
generateBricks = do
  let px = [0,100 .. 1200]
  let py0 = replicate (length px) 0
  let py1 = replicate (length px) 50
  let py2 = replicate (length px) 100
  let py3 = replicate (length px) 150
  let py4 = replicate (length px) 200

  let bs0 = mapBricks px py0
  let bs1 = mapBricks px py1
  let bs2 = mapBricks px py2
  let bs3 = mapBricks px py3
  let bs4 = mapBricks px py4

  bs0 ++ bs1 ++ bs2 ++ bs3 ++ bs4