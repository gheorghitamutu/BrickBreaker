{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified SDL
import qualified SDL.Font

import SDL.Framerate
import Prelude hiding(Left, Right)

import Game
import Settings


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  w <- SDL.createWindow "Brick Breaker" SDL.defaultWindow { SDL.windowInitialSize = Settings.windowSizes }
  r <- SDL.createRenderer w (-1) SDL.RendererConfig
                                           { SDL.rendererType = SDL.AcceleratedRenderer
                                           , SDL.rendererTargetTexture = False
                                           }
  SDL.showWindow w

  SDL.Framerate.with Settings.fps $ loop r initialWorld

  SDL.destroyWindow w
  SDL.Font.quit
  SDL.quit
