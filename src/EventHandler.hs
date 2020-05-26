module EventHandler (Intent(..), eventToIntent, shouldQuit, actionHandler) where

import SDL hiding(Unknown)
import Prelude hiding (Left, Right)
import World
import Paddle

data Intent = Quit
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | Unknown


actionHandler :: Intent -> World -> World
actionHandler Quit w = w
actionHandler MoveRight w = changePaddleDirection Paddle.Right w
actionHandler MoveUp w = changePaddleDirection Paddle.Up w
actionHandler MoveLeft w = changePaddleDirection Paddle.Left w
actionHandler MoveDown w = changePaddleDirection Paddle.Down w
actionHandler Unknown w = w
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
    SDL.KeycodeUp     -> MoveUp
    SDL.KeycodeDown   -> MoveDown
    SDL.KeycodeLeft   -> MoveLeft
    SDL.KeycodeRight  -> MoveRight
    _                 -> Unknown

shouldQuit :: Intent -> Bool
shouldQuit Quit = True
shouldQuit _ = False
