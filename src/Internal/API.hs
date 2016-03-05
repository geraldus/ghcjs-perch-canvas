module Internal.API where

import           Internal.FFI
import           Internal.Type

import           Data.JSString.Text (textToJSString)
import           GHCJS.Marshal      (FromJSVal (..))
import           GHCJS.Perch        (Elem)

import           Control.Arrow      ((***))
import           Data.Text          (Text)


-- | Get @2d@ context of Canvas element.  Throws run-time error if given element
-- is not a @CANVAS@.
canvas2dCtx :: Elem -> IO CanvasCtx
canvas2dCtx = canvasCtx ctxName2d

-- * Drawing Rectangles

-- | Set all pixels in rectangle to transparent black erasing old contents.
clearRect :: Coords -> Pixels -> Pixels -> CanvasRender ()
clearRect c w h = CanvasRender $ \ctx ->
  do let (rx, ry) = fromCoords c
         w' = pix w
         h' = pix h
     js_canvasContext2dClearRect (getContext ctx) rx ry w' h'
     return ctx

-- | Paint a rectangle using current stroke style.
strokeRect :: Coords -- ^ rectangle starting point
           -> Pixels -- ^ rectangle width
           -> Pixels -- ^ rectangle height
           -> CanvasRender ()
strokeRect c w h = CanvasRender $ \ctx ->
  do let (rx, ry) = fromCoords c
         w' = pix w
         h' = pix h
     js_canvasContext2dStrokeRect (getContext ctx) rx ry w' h'
     return ctx

-- * Drwing Text

-- | Fill text.
fillText :: Text -- ^ text to render
         -> Pixels -- ^ x
         -> Pixels -- ^ y
         -> Maybe Pixels -- ^ optional max width
         -> CanvasRender ()
fillText txt x y mw = CanvasRender $ \ctx ->
  do let t = textToJSString txt
         c = getContext ctx
     case mw of
       Nothing -> js_canvasContext2dFillTextA3 c t (pix x) (pix y)
       Just w -> js_canvasContext2dFillTextA4 c t (pix x) (pix y) (pix w)
     return ctx


-- * Paths
-- The following methods can be used to manipulate paths of objects.

-- | Starts a new path by emptying the list of sub-paths. Call this method when
-- you want to create a new path.
beginPath :: CanvasRender ()
beginPath = CanvasRender $ \ctx ->
  do js_canvasContext2dBeginPath (getContext ctx)
     return ctx

-- | Causes the point of the pen to move back to the start of the current
-- sub-path. It tries to draw a straight line from the current point to the
-- start. If the shape has already been closed or has only one point, this
-- function does nothing.
closePath :: CanvasRender ()
closePath = CanvasRender $ \ctx ->
  do js_canvasContext2dClosePath (getContext ctx)
     return ctx


-- * Drawing Images

-- | Set context images smoothing.  Note, this is experimental feature and may
-- not work properly in some cases.
setImageSmoothing :: Bool -> CanvasRender ()
setImageSmoothing v = CanvasRender $ \ctx ->
  do js_canvasContext2dSetImageSmoothing (getContext ctx) v
     return ctx

-- | Handy shortcut for enabling image smoothing.
enableImageSmoothing :: CanvasRender ()
enableImageSmoothing = setImageSmoothing True
-- | Handy shortcut for disabling image smoothing.
disableImageSmoothing :: CanvasRender ()
disableImageSmoothing = setImageSmoothing False


-- | Draw an image.
drawImage :: CanvasDrawImageSettings -> CanvasRender ()
drawImage (DrawImageSimple (CanvasImageSource is) dx dy) = CanvasRender $
  \ctx ->
    do js_canvasContext2dDrawImageA3 (getContext ctx) is (pix dx) (pix dy)
       return ctx
drawImage (DrawImageSized (CanvasImageSource is) dx dy w h) = CanvasRender $
  \ctx ->
    do js_canvasContext2dDrawImageA5
         (getContext ctx) is (pix dx) (pix dy) (pix w) (pix h)
       return ctx
drawImage (DrawImageCropped
            (CanvasImageSource is) sx sy sWidth sHeight dx dy dWidth dHeight) =
  CanvasRender $ \ctx ->
    do let (CanvasCtx c) = ctx
           d = pix sx
           e = pix sy
           f = pix sWidth
           g = pix sHeight
           h = pix dx
           i = pix dy
           j = pix dWidth
           k = pix dHeight
       js_canvasContext2dDrawImageA9 c is d e f g h i j k
       return ctx


-- | Convert integral value to 'Pixels'.
pixels :: Int -> Pixels
pixels = Px

-- | Convert 'Pixels' to integral value.
pix :: Pixels -> Int
pix (Px n) = fromIntegral n


-- * Internal Stuff

-- |Canvas two-dimentional context name.
ctxName2d :: Text
ctxName2d = "2d"

-- | Get context of @CANVAS@ element.  __Unsafe__ code, throws run-time
-- exception if context is not available (for example when element is not a
-- @CANVAS@).
canvasCtx :: Text -- ^ context name
          -> Elem -- ^ element, supposed to be @CANVAS@
          -> IO CanvasCtx
canvasCtx n e =
  do isCanvas <- js_elementIsCanvas e
     -- FIXME Check Bool return tile, maybe conversion JSBool -> Bool needed
     if not isCanvas
        then error "Context is not available - not a CANVAS."
        else do j <- js_canvasGetContext e (textToJSString n)
                v <- fromJSVal j
                case v of
                  Nothing -> error "Error converting context to Elem"
                  Just e -> return $ CanvasCtx e


-- | Do action with 'CanvasCtx' and return it.
withCtx :: (Elem -> IO ()) -> CanvasCtx -> IO CanvasCtx
withCtx io c =
  do io (getContext c)
     return c

-- | Unwrap canvas element from 'CanvasCtx'.
getContext :: CanvasCtx -> Elem
getContext (CanvasCtx e) = e


-- | Convert 'Elem' to 'CanvasImageSource'.  Valid elements are:
-- @IMAGE@, @VIDEO@, @CANVAS@, @CanvasRenderingContext2D@. @ImageBitmap@ is
-- currently unsupported.  __Unsafe__ code, throws run-time exception if element
-- is notvalid image source.
makeImageSource :: Elem -> IO CanvasImageSource
makeImageSource e =
  do isSource <- js_elementIsImageSource e
     -- FIXME Check if Bool is proper return type
     if not isSource
        then error "Invalid image source."
        else return (CanvasImageSource e)

-- | Unwrap image source.
getImageSource :: CanvasImageSource -> Elem
getImageSource (CanvasImageSource s) = s


-- | Make 'Coords' from X and Y 'Pixels'.
makeCoords :: Pixels -> Pixels -> Coords
makeCoords px py = Coords (px, py)

-- | Unwrap 'Coords' to (X, Y) 'Pixels' tuple.
takeCoords :: Coords -> (Pixels, Pixels)
takeCoords (Coords (px, py)) = (px, py)

-- | Unwrap 'Coords' to (X, Y) 'Int' tuple.
fromCoords :: Coords -> (Int, Int)
fromCoords = (pix *** pix) . takeCoords
