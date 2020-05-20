module EventHandler (Intent(..), eventToIntent, shouldQuit, actionHandler) where

import SDL hiding(Unknown)
import Prelude hiding (Left, Right)
import World

data Intent = Quit
  | Left
  | Right
  | Up
  | Down
  | Unknown


actionHandler :: Intent -> World -> World
actionHandler Unknown w = w
actionHandler Quit w = w
actionHandler Left w = w
-- TODO: handle paddle movement
-- TODO: generate a new world accordingly

eventToIntent :: Maybe SDL.Event -> Intent
eventToIntent = maybe Unknown (payloadToIntent . extractPayload)

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = getKey k
payloadToIntent _                     = Unknown


getKey :: SDL.KeyboardEventData -> Intent
getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Unknown
getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Unknown
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeUp     -> Up
    SDL.KeycodeDown   -> Down
    SDL.KeycodeLeft   -> Left
    SDL.KeycodeRight  -> Right
    _                 -> Unknown

shouldQuit :: Intent -> Bool
shouldQuit Quit = True
shouldQuit _ = False
