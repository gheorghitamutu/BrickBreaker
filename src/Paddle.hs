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

getPaddlePosition :: Paddle -> V2 CInt
getPaddlePosition (Paddle p _ _ _) = p

getPaddleSize:: Paddle -> V2 CInt
getPaddleSize (Paddle _ s _ _) = s

getPaddleColor :: Paddle -> V4 Word8
getPaddleColor (Paddle _ _ c _) = c

isPaddleHidden :: Paddle -> Bool
isPaddleHidden (Paddle _ _ _ h) = h

drawPaddle :: (MonadIO m) => Paddle -> Renderer -> m ()
drawPaddle p r = SDL.Primitive.fillRectangle r (getPaddleSize p) (getPaddlePosition p) (getPaddleColor p)