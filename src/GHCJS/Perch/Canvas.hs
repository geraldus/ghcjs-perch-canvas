module GHCJS.Perch.Canvas
  ( -- * Canvas Rendering Monad
    CanvasRender (render)
  , CanvasCtx
  , canvas2dCtx
  , getContext
    -- ** Drawing Rectangles
  , clearRect
  , strokeRect
    -- ** Drawing Text
  , fillText
  , setContextFont
  , getContextFont
    -- ** Paths
  , beginPath
  , closePath
    -- ** Drawing Images
  , makeImageSource
  , getImageSource
  , drawImage
  , setImageSmoothing
  , enableImageSmoothing
  , disableImageSmoothing
    -- ** Miscellaneous
  , pix
  , pixels
  , makeCoords
  , takeCoords
  , fromCoords
    -- ** Types
  , CanvasDrawImageSettings (..)
  , CanvasImageSource
  , Pixels
  , Coords
  ) where

import           Internal.API
import           Internal.Type
