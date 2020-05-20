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

drawBrick :: (MonadIO mBrick) => Brick -> Renderer -> mBrick ()
drawBrick b r = SDL.Primitive.fillRectangle r (brickSize b) (brickPosition b) (brickColor b)
