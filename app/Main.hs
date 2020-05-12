{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Vect (V2(..), V4(..))
import Control.Monad (unless)
import Foreign.C.Types (CInt)
import Prelude hiding (Left, Right)

-- mine
import qualified Colours (white, black)
import qualified Structures(Intent, Direction, SurfaceMap, surfacePaths, selectSurface)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 720)

main :: IO ()
main = do
  SDL.initializeAll
  
  screen <- SDL.getWindowSurface w
      surfaces <- mapM SDL.loadBMP surfacePaths
  
      let doRender = C.renderSurfaceToWindow w screen
      doRender (help surfaces)
  
      whileM $
        mkIntent <$> SDL.pollEvent
        >>= runIntent surfaces doRender
  
      mapM_ SDL.freeSurface surfaces
      SDL.freeSurface screen

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

  render renderer

  clear renderer
  present renderer

  unless exitWanted (appLoop renderer maybeNotSwitch)

render :: SDL.Renderer -> IO ()
render renderer = do
  SDL.clear renderer
  SDL.drawRect renderer (Just $ SDL.Rectangle (P $ V2 380 280) (V2 40 40))
  SDL.present renderer

mkIntent :: Maybe SDL.Event -> Intent
mkIntent = maybe Idle (payloadToIntent . extractPayload)

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Idle

getKey :: SDL.KeyboardEventData -> Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Idle
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeUp     -> SelectSurface Up
    SDL.KeycodeDown   -> SelectSurface Down
    SDL.KeycodeLeft   -> SelectSurface Left
    SDL.KeycodeRight  -> SelectSurface Right
    _                 -> SelectSurface Help

runIntent :: (Monad m) => SurfaceMap a -> (a -> m ()) -> Intent -> m Bool
runIntent _ _ Quit
  = pure False

runIntent _ _ Idle
  = pure True

runIntent cs f (SelectSurface key)
  = True <$ f (selectSurface key cs)