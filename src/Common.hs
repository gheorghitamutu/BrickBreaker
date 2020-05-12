module Common where

import qualified SDL
import qualified SDL.Image

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)

import Foreign.C.Types        (CInt)

import SDL (($=))


withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit


withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit


withWindow :: (MonadIO m) => Text -> SDL.V2 CInt -> (SDL.Window -> m a) -> m ()
withWindow title s op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

    where
      p = SDL.defaultWindow { SDL.windowInitialSize = s }


withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r


rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }


renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow w s i
  = SDL.surfaceBlit i Nothing s Nothing
  >> SDL.updateWindowSurface w


shouldContinue :: Maybe SDL.Event -> Bool
shouldContinue = maybe True (not . isQuitEvent)


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = True <$ f
conditionallyRun _ False = pure False


isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _ = False


setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest


loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)


mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)


mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h


fillRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = SDL.fillRect r (Just s)


drawRectangle :: (MonadIO m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
drawRectangle r s = SDL.drawRect r (Just s)