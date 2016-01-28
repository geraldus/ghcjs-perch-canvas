module Internal.Type
  ( -- * Canvas rendering monad
    CanvasRender (..)
    -- * Low-level primitives
  , CanvasCtx (..)
  , CanvasDrawImageSettings (..)
  , CanvasImageSource (..)
  , Pixels (..)
  , Coords (..)
  , TagName (..)
  ) where

import           GHCJS.Perch            (Elem)

import           Control.Applicative    (Applicative (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Monoid            (Monoid (..))
import           Unsafe.Coerce          (unsafeCoerce)

-- | Perch like canvas renderer.
newtype CanvasRender a = CanvasRender { render :: CanvasCtx -> IO CanvasCtx }

-- | Canvas context representation.
newtype CanvasCtx = CanvasCtx Elem

-- | Pixel representaion.
newtype Pixels = Px Int deriving (Eq, Ord)

-- | Coordinates representaion.
newtype Coords = Coords (Pixels, Pixels)

-- | TagName wrapper.
newtype TagName = Tag String

-- | 'drawImage' primitive settings.  There are three options when calling
-- @drawImage()@ funciton:
--
-- [@simple@] @ctx.drawImage(image, dx, dy);@
--
-- [@sized@] @ctx.drawImage(image, dx, dy, dWidth, dHeight);@
--
-- [@cropped@] @ctx.drawImage(image, sx, sy, sWidth, sHeight, dx, dy, dWidth,
-- dHeight);@
--
-- See
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawImage reference>
-- for more details.
data CanvasDrawImageSettings
  = DrawImageSimple
      CanvasImageSource Pixels Pixels
      -- ^ simple settings
  | DrawImageSized
      CanvasImageSource Pixels Pixels Pixels Pixels
      -- ^ size manipulation
  | DrawImageCropped
      CanvasImageSource Pixels Pixels Pixels Pixels Pixels Pixels Pixels Pixels
      -- ^ crop image and modify size


-- | Wrapper type representing objects which can be used as image sources.
newtype CanvasImageSource = CanvasImageSource Elem


instance Functor CanvasRender

instance Applicative CanvasRender

instance Monoid (CanvasRender a) where
  mappend rx ry = CanvasRender $ \c ->
    do render rx c
       render ry c
       return c
  mempty = CanvasRender return

instance Monad CanvasRender where
  return = mempty
  (>>) x y = mappend (unsafeCoerce x) y
  (>>=) = error "Bind (>>=) invokation in the CanvasRender Monad."

instance MonadIO CanvasRender where
  liftIO io = CanvasRender $ \ctx -> io >> return ctx


instance Num Pixels where
  (Px x) + (Px y) = Px (x + y)
  (Px x) * (Px y) = Px (x * y)
  abs (Px x) = Px (abs x)
  signum (Px x) = Px (signum x)
  negate (Px x) = Px (negate x)
  fromInteger x = Px (fromInteger x)
