{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Vect (V2(..), V4(..))
import Control.Monad (unless)
import Foreign.C.Types (CInt)

-- mine
import qualified Colours (white, black)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 720)

main :: IO ()
main = do
  initialize [SDL.InitVideo]
  window <- createWindow "BrickBreaker" defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer True

appLoop :: Renderer -> Bool -> IO ()
appLoop renderer switch = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False

  let eventIsSPress event =
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeS
            _ -> False

  let exitWanted = any eventIsQPress events || elem SDL.QuitEvent (map SDL.eventPayload events)

  let backgroundColor = if switch then Colours.black else Colours.white
  let maybeNotSwitch = if any eventIsSPress events then not switch else switch

  rendererDrawColor renderer $= backgroundColor

  clear renderer
  present renderer

  unless exitWanted (appLoop renderer maybeNotSwitch)