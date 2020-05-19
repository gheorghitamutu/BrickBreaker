module Brick where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)

data Brick = Brick
  {
    brickPosition :: V2 CInt,
    brickSize :: V2 CInt,
    brickColor :: V4 Word8,
    brickHidden :: Bool
  }

getBrickPosition :: Brick -> V2 CInt
getBrickPosition (Brick p _ _ _) = p

getBrickSize:: Brick -> V2 CInt
getBrickSize (Brick _ s _ _) = s

getBrickColor :: Brick -> V4 Word8
getBrickColor (Brick _ _ c _) = c

isBrickHidden :: Brick -> Bool
isBrickHidden (Brick _ _ _ h) = h

drawBrick :: (MonadIO mBrick) => Brick -> Renderer -> mBrick ()
drawBrick b r = SDL.Primitive.fillRectangle r (getBrickSize b) (getBrickPosition b) (getBrickColor b)
