{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Structures where (Intent, Direction, SurfaceMap, surfacePaths, selectSurface)

import Control.Monad.Extra (whileM)
import Prelude hiding (Left, Right)

data Intent
  = SelectSurface Direction
  | Idle
  | Quit


data Direction
  = Help
  | Up
  | Down
  | Left
  | Right


data SurfaceMap a = SurfaceMap
  { help  :: a
  , up    :: a
  , down  :: a
  , left  :: a
  , right :: a
  } deriving (Foldable, Traversable, Functor)

surfacePaths :: SurfaceMap FilePath
surfacePaths = SurfaceMap
  { help  = "./assets/press.bmp"
  , up    = "./assets/up.bmp"
  , down  = "./assets/down.bmp"
  , left  = "./assets/left.bmp"
  , right = "./assets/right.bmp"
  }

selectSurface :: Direction -> SurfaceMap a -> a
selectSurface Help  = help
selectSurface Up    = up
selectSurface Down  = down
selectSurface Left  = left
selectSurface Right = right