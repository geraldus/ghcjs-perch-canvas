module Internal.FFI where

import           Data.JSString (JSString)
import           GHCJS.Perch   (Elem)
import           GHCJS.Types   (JSVal)


-- | Check if the element is a @CNAVAS@ by comparing constructors.
foreign import javascript unsafe
  "$r = (function () { \
  \  var c = document.createElement('canvas'); \
  \  if ($1.constructor === c.constructor) { \
  \    return true; \
  \  } else { \
  \    return false; \
  \  } \
  \}());"
  js_elementIsCanvas :: Elem -> IO Bool

-- | Get canvas context.
foreign import javascript unsafe "$1.getContext($2)"
  js_canvasGetContext :: Elem -> JSString -> IO JSVal

-- * Drawing Rectangles

-- | Paint a rectangle using the current stroke style.
foreign import javascript unsafe "$1.strokeRect($2, $3, $4, $5)"
  js_canvasContext2dStrokeRect :: Elem -> Int -> Int -> Int -> Int -> IO ()

-- | Set all pixels in rectangle to transparent black erasing old contents.
foreign import javascript unsafe "$1.clearRect($2, $3, $4, $5)"
  js_canvasContext2dClearRect :: Elem -> Int -> Int -> Int -> Int -> IO ()


-- * Paths

-- | Begin path.
foreign import javascript unsafe "$1.beginPath()"
  js_canvasContext2dBeginPath :: Elem -> IO ()

-- | Close path.
foreign import javascript unsafe "$1.closePath()"
  js_canvasContext2dClosePath :: Elem -> IO ()


-- * Drawing Text

-- | Fill text, simple variant.
foreign import javascript unsafe
  "$1.fillText($2, $3, $4)"
  js_canvasContext2dFillTextA3 :: Elem -- ^ context
                               -> JSString -- ^ text to render
                               -> Int -- ^ x
                               -> Int -- ^ y
                               -> IO ()

-- | Fill text, max width variant.
foreign import javascript unsafe
  "$1.fillText($2, $3, $4)"
  js_canvasContext2dFillTextA4 :: Elem -- ^ context
                               -> JSString -- ^ text to render
                               -> Int -- ^ x
                               -> Int -- ^ y
                               -> Int -- ^ max width
                               -> IO ()


-- * Drawing Images

-- | Check if the element is a valid image source by compairing constructors.
-- Valid image sources are @IMAGE@, @VIDEO@, @CANVAS@, or
-- @CanvasRenderingContext2D@.  @ImageBitmap@ is currently unsupported.
foreign import javascript unsafe
  "$r = (function () { \
  \  var i = document.createElement('img'); \
  \  var v = document.createElement('video'); \
  \  var c = document.createElement('canvas'); \
  \  var ct = c.getContext('2d'); \
  \  var x = $1.constructor; \
  \  var res = (x === i.constructor || \
  \             x === v.constructor || \
  \             x === c.constructor || \
  \             x === ct.constructor); \
  \  return res; \
  \}());"
  js_elementIsImageSource :: Elem -> IO Bool


-- | Set context image smoothing.
foreign import javascript unsafe
  "(function () { \
  \  $1.mozImageSmoothingEnabled = $2; \
  \  $1.webkitImageSmoothingEnabled = $2; \
  \  $1.msImageSmoothingEnabled = $2; \
  \  $1.imageSmoothingEnabled = $2; \
  \}());"
  js_canvasContext2dSetImageSmoothing :: Elem -> Bool -> IO ()


-- | Draw image, simple variant.
foreign import javascript unsafe "$1.drawImage($2, $3, $4)"
  js_canvasContext2dDrawImageA3 :: Elem -- ^ context
                                -> Elem -- ^ image source
                                -> Int  -- ^ dx
                                -> Int  -- ^ dy
                                -> IO ()

-- | Draw image, sized variant.
foreign import javascript unsafe "$1.drawImage($2, $3, $4, $5, $6)"
  js_canvasContext2dDrawImageA5 :: Elem -- ^ context
                                -> Elem -- ^ image source
                                -> Int  -- ^ dx
                                -> Int  -- ^ dy
                                -> Int  -- ^ dWidth
                                -> Int  -- ^ dHeight
                                -> IO ()

-- | Draw image, crop and size variant.
foreign import javascript unsafe
  "$1.drawImage($2, $3, $4, $5, $6, $7, $8, $9, $10)"
  js_canvasContext2dDrawImageA9 :: Elem -- ^ context
                                -> Elem -- ^ image source
                                -> Int  -- ^ sx
                                -> Int  -- ^ sy
                                -> Int  -- ^ sWidth
                                -> Int  -- ^ sHeight
                                -> Int  -- ^ dx
                                -> Int  -- ^ dy
                                -> Int  -- ^ dWidth
                                -> Int  -- ^ dHeight
                                -> IO ()
