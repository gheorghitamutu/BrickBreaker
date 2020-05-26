module Ball where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (Left, Right, LT)

data Direction = LD
  | LT
  | RD
  | RT
  | Idle

data Ball = Ball
  {
    ballPosition :: V2 CInt,
    ballRadius :: CInt,
    ballColor :: V4 Word8,
    ballHidden :: Bool,
    ballVelocity :: CInt,
    ballDirection :: Direction
  }

drawBall :: (MonadIO m) => Ball -> Renderer -> m ()
drawBall b r = SDL.Primitive.fillCircle r (ballPosition b) (ballRadius b) (ballColor b)

updateBall :: Ball -> Ball
updateBall b =
  Ball
    (getNextPosition (ballPosition b) (ballVelocity b) (ballDirection b) (ballRadius b))
    (ballRadius b)
    (ballColor b)
    (ballHidden b)
    (ballVelocity b)
    (getNextDirection (ballPosition b) (ballVelocity b) (ballDirection b) (ballRadius b))

getNextPosition :: V2 CInt -> CInt -> Direction -> CInt -> V2 CInt
getNextPosition (V2 x y) v d r = do
  let lt = V2 (x - r) (y - r)
  let rd = V2 (x + r) (y + r)

  case d of
    LT
      | not (hitsTheScreenLimits (lt - V2 v v) rd) -> V2 (x - v) (y - v)
      | hitsLeft (lt - V2 v 0) rd -> V2 r y
      | otherwise -> V2 x r
    RT ->
      if not (hitsTheScreenLimits (lt + V2 v v) (rd - V2 v v))
        then V2 (x + v) (y - v)
        else V2 x y
    LD ->
      if not (hitsTheScreenLimits (lt - V2 v v) (rd + V2 v v))
        then V2 (x - v) (y + v)
        else V2 x y
    RD
      | not (hitsTheScreenLimits (lt + V2 v v) (rd + V2 v v)) -> V2 (x + v) (y + v)
      | hitsRight lt (rd + V2 v 0) -> V2 r y
      | otherwise -> V2 x r
    Idle -> V2 x y


getNextDirection :: V2 CInt -> CInt -> Direction -> CInt -> Direction
getNextDirection (V2 x y) v d r = do
  let lt = V2 (x - r) (y - r)
  let rd = V2 (x + r) (y + r)

  case d of
    LT
      | hitsLeft (lt - V2 v 0) rd && hitsTop (lt - V2 0 v) rd -> RD
      | hitsLeft (lt - V2 v 0) rd -> RT
      | hitsTop (lt - V2 0 v) rd -> LD
      | otherwise -> LT
    RT
      | hitsRight lt (rd + V2 0 v) && hitsTop (lt - V2 0 v) rd -> LD
      | hitsRight lt (rd + V2 0 v) -> LT
      | hitsTop (lt - V2 0 v) (rd + V2 0 v) -> RD
      | otherwise -> RT
    LD
      | hitsLeft (lt - V2 v 0) rd && hitsDown (lt - V2 0 v) (rd + V2 0 v) -> RT
      | hitsLeft (lt - V2 v 0) rd -> RD
      | hitsDown (lt - V2 0 v) (rd + V2 0 v) -> Idle
      | otherwise -> LD
    RD
      | hitsRight lt (rd + V2 0 v) && hitsDown lt (rd + V2 v v) -> LT
      | hitsRight lt (rd + V2 0 v) -> LD
      | hitsDown lt (rd + V2 v v) -> Idle
      | otherwise -> RD
    Idle -> Idle

-- screen limits
hitsLeft :: V2 CInt -> V2 CInt -> Bool
hitsLeft (V2 al au) (V2 ar ad) = al <= 0

hitsTop :: V2 CInt -> V2 CInt -> Bool
hitsTop (V2 al au) (V2 ar ad) = au <= 0

hitsRight :: V2 CInt -> V2 CInt -> Bool
hitsRight (V2 al au) (V2 ar ad) = ar >= 1280

hitsDown :: V2 CInt -> V2 CInt -> Bool
hitsDown (V2 al au) (V2 ar ad) = au >= 720

hitsTheScreenLimits :: V2 CInt -> V2 CInt -> Bool
hitsTheScreenLimits (V2 al au) (V2 ar ad) = al <= 0 || ar <= 0 -- || al >= 1280 || ar >= 1280 || au >= 720 || ad >= 720