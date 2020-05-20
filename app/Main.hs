{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified SDL

import SDL.Framerate
import Prelude hiding(Left, Right)

import Game
import Settings


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  w <- SDL.createWindow "Brick Breaker" SDL.defaultWindow { SDL.windowInitialSize = Settings.windowSizes }
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  SDL.showWindow w

  SDL.Framerate.with Settings.fps $ loop r initialWorld

  SDL.destroyWindow w
  SDL.quit
