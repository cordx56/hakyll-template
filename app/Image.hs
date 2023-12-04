{-# LANGUAGE FlexibleContexts #-}

module Image where

import Codec.Picture (PixelRGBA8 (..), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType

blackColor = PixelRGBA8 17 17 17 255
whiteColor = PixelRGBA8 204 204 204 255
redColor = PixelRGBA8 204 34 34 255

imageWidth = 1024 :: Float
imageHeight = 512 :: Float
margin = 20
textHeight = 64
dpi = 300

drawTextRawPoint font x y pointSize color text =
  withTexture (uniformTexture color)
    $ printTextAt font pointSize (V2 x y) text

textSplitHead :: Font -> Dpi -> PointSize -> Float -> String -> (String, String)
textSplitHead font dpi pointSize maxWidth text =
  if maxWidth < currentWidth
    then (ns, remain ++ [last text])
    else (text, "")
 where
  getBoundingBox = stringBoundingBox font dpi pointSize
  currentBounding = getBoundingBox text
  currentWidth = _xMax currentBounding - _xMin currentBounding
  (ns, remain) = textSplitHead font dpi pointSize maxWidth (init text)

textSplit :: Font -> Dpi -> PointSize -> Float -> String -> [String]
textSplit font dpi pointSize maxWidth text = "a"

textSplitAndBoundingBox :: Font -> Dpi -> PointSize -> Float -> String
textSplitAndBoundingBox font dpi pointSize maxWidth text =
  textSplitAndBoundingBox
 where
  getBoundingBox = stringBoundingBox font dpi pointSize

drawText font x y size color text =
  drawTextRawPoint font (x - offsetX) (y + offsetY) pointSize color text
 where
  pointSize = pixelSizeInPointAtDpi textHeight dpi
  getBoundingBox = stringBoundingBox font dpi pointSize
  offsetX = (_xMax boundingBox - _xMin boundingBox) / 2
  offsetY = (_yMax boundingBox - _yMin boundingBox) / 2

draw font siteTitle pageTitle =
  renderDrawingAtDpi (round imageWidth) (round imageHeight) dpi blackColor $ do
    drawText font (imageWidth / 2) (imageHeight / 2 - textHeight / 2) textHeight whiteColor pageTitle
    drawText font (imageWidth / 2) (imageHeight - margin - textHeight) textHeight redColor siteTitle

generate fontFp siteTitle pageTitle fp = do
  fontErr <- loadFontFile fontFp
  case fontErr of
    Left err -> error err
    Right font -> writePng fp $ draw font siteTitle pageTitle
