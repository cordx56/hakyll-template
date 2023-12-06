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

lineMargin = 10

textHeight = 64

dpi = 300

drawTextRawPoint font x y pointSize color text =
  withTexture (uniformTexture color) $
    printTextAt font pointSize (V2 x y) text

data PosSize = PosSize {_posX :: Float, _posY :: Float, _sizeW :: Float, _sizeH :: Float}

data CharDrawPlan = CharDrawPlan {_cPosSize :: PosSize, _cPlan :: Char, _cPointSize :: PointSize, _cFont :: Font}

newtype TextDrawPlan = TextDrawPlan {_charPlans :: [CharDrawPlan]}

justifyPlan :: TextDrawPlan -> TextDrawPlan
justifyPlan plan =
  TextDrawPlan $ justifyXY (-yMin) (-xMin)
  where
    charPlans = _charPlans plan
    charPlanPosSizes = map _cPosSize charPlans
    yPoss = map _posY charPlanPosSizes
    yMin = minimum yPoss
    xPoss = map _posX charPlanPosSizes
    xMin = minimum xPoss
    justifyXY x y =
      map (\p -> CharDrawPlan (posSizeJustify $ _cPosSize p) (_cPlan p) (_cPointSize p) (_cFont p)) charPlans
      where
        posSizeJustify ps = PosSize (_posX ps + x) (_posY ps + y) (_sizeW ps) (_sizeH ps)

planSize :: TextDrawPlan -> (Float, Float)
planSize plan =
  (maximum ws, maximum hs)
  where
    justified = _charPlans $ justifyPlan plan
    posSizes = map _cPosSize justified
    ws = map _sizeW posSizes
    hs = map _sizeH posSizes

drawCharPlan x y color plan =
  withTexture (uniformTexture color) $
    printTextAt (_cFont plan) (_cPointSize plan) (V2 (x - _posX ps) (y - _posY ps)) [_cPlan plan]
  where
    ps = _cPosSize plan

drawTextPlan x y color plan =
  drawCharPlans $ _charPlans plan
  where
    drawCharPlans (p : ps) = do
      drawCharPlan x y color p
      drawCharPlans ps
    drawCharPlans [] = return ()

planDrawText :: Font -> Dpi -> PointSize -> String -> TextDrawPlan
planDrawText font dpi pointSize text =
  TextDrawPlan (drawChars 0 0 text)
  where
    drawChars x y (c : cs) = CharDrawPlan ps c pointSize font : drawChars (x + sW) y cs
      where
        boundingBox = stringBoundingBox font dpi pointSize [c]
        pX = x - _xMin boundingBox
        pY = y - _yMin boundingBox
        sW = _xMax boundingBox - _xMin boundingBox
        sH = _yMax boundingBox - _yMin boundingBox
        ps = PosSize pX pY sW sH
    drawChars _ _ [] = []

splitTextHead :: Font -> Dpi -> PointSize -> Float -> String -> (String, String)
splitTextHead font dpi pointSize maxWidth text =
  if maxWidth < currentWidth
    then (ns, remain ++ [last text])
    else (text, "")
  where
    getPlan = planDrawText font dpi pointSize
    currentPlan = _charPlans $ getPlan text
    currentWidth = _posX lps + _sizeW lps
      where
        lps = _cPosSize $ last currentPlan
    (ns, remain) = splitTextHead font dpi pointSize maxWidth (init text)

splitText :: Font -> Dpi -> PointSize -> Float -> String -> [String]
splitText font dpi pointSize maxWidth =
  split []
  where
    splitHead = splitTextHead font dpi pointSize maxWidth
    split texts [] = texts
    split texts remain = split (texts ++ [newH]) newRemain
      where
        (newH, newRemain) = splitHead remain

drawText font x y size color text = do
  drawLines 0 plans
  where
    pointSize = pixelSizeInPointAtDpi textHeight dpi

    getPlan = planDrawText font dpi pointSize
    splitted = splitText font dpi pointSize (imageWidth - margin * 6) text
    plans = map getPlan splitted
    jusitified = map justifyPlan plans
    lineMarginSum = lineMargin * (fromIntegral $ length splitted - 1 :: Float)
    heightSum =
      sum (map snd sizes) + lineMarginSum
      where
        sizes = map planSize jusitified
    offsetSumY = heightSum / 2
    drawLines currentHeight (p : ps) = do
      drawTextPlan (x - offsetX) (y - offsetSumY + currentHeight + offsetY) color p
      drawLines (currentHeight + h + lineMargin) ps
      where
        (w, h) = planSize p
        offsetX = w / 2
        offsetY = h / 2
    drawLines _ [] = return ()

-- Draw titles
draw font siteTitle pageTitle =
  renderDrawingAtDpi (round imageWidth) (round imageHeight) dpi blackColor $ do
    drawText font (imageWidth / 2) (imageHeight / 2 - textHeight / 2) textHeight whiteColor pageTitle
    drawText font (imageWidth / 2) (imageHeight - margin - textHeight) textHeight redColor siteTitle

generate fontFp siteTitle pageTitle fp = do
  fontErr <- loadFontFile fontFp
  case fontErr of
    Left err -> error err
    Right font -> writePng fp $ draw font siteTitle pageTitle
