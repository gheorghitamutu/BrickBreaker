{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.IO.Class (MonadIO)
import SDL                    (($=))
import Control.Monad.Extra (whileM)

import Game

main :: IO ()
main =
  C.withSDL $ do
    C.setHintQuality
    C.withWindow "Brick Breaker" Game.windowSizes $ \w ->
      C.withRenderer w $ \r -> whileM $ C.shouldContinue <$> SDL.pollEvent >>= C.conditionallyRun (gameLoop r)

gameLoop :: SDL.Renderer -> IO ()
gameLoop = draw -- TODO: should add update and handle input and event here as well!

draw :: SDL.Renderer -> IO ()
draw renderer = do
  clearScreen renderer
  drawWorld initialWorld renderer
  SDL.present renderer

clearScreen :: (MonadIO m) => SDL.Renderer -> m ()
clearScreen r = do
  SDL.rendererDrawColor r $= SDL.V4 255 255 255 100
  SDL.clear r
