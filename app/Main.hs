{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import qualified SDL

import Control.Monad.IO.Class (MonadIO)
import SDL                    (($=))
import SDL.Framerate
import Prelude                hiding(Left, Right)
import EventHandler (eventToIntent, shouldQuit, actionHandler)

import Game
import Settings
import World
import Control.Monad.Cont (unless)


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  w <- SDL.createWindow "Brick Breaker" SDL.defaultWindow { SDL.windowInitialSize = Settings.windowSizes }
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  SDL.showWindow w

  let fps = 60

  SDL.Framerate.with fps $ gameLoop r initialWorld

  SDL.destroyWindow w
  SDL.quit


gameLoop :: SDL.Renderer -> World -> SDL.Framerate.Manager -> IO ()
gameLoop r w fpsm = do
      event <- SDL.pollEvent
      let action = eventToIntent event
      let quit = shouldQuit action

      let newWorld = actionHandler action w
      draw r w

      SDL.Framerate.delay_ fpsm

      unless quit (gameLoop r newWorld fpsm)

draw :: SDL.Renderer -> World -> IO ()
draw r w = do
  clearScreen r
  drawWorld w r
  SDL.present r

clearScreen :: (MonadIO m) => SDL.Renderer -> m ()
clearScreen r = do
  SDL.rendererDrawColor r $= SDL.V4 255 255 255 100
  SDL.clear r
