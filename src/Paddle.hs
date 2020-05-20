module Paddle where

import SDL
import qualified SDL.Primitive
import Foreign.C.Types (CInt)
import Data.Word
import Control.Monad.IO.Class (MonadIO)

data Paddle = Paddle
  {
    paddlePosition :: V2 CInt,
    paddleSize :: V2 CInt,
    paddleColor :: V4 Word8,
    paddleHidden :: Bool
  }

drawPaddle :: (MonadIO m) => Paddle -> Renderer -> m ()
drawPaddle p r = SDL.Primitive.fillRectangle r (paddleSize p) (paddlePosition p) (paddleColor p)