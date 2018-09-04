--- Copyright 2018 The Australian National University, All rights reserved
module View where

import CodeWorld hiding (Point, circle, polygon, rectangle)
import Data.Text (pack)
import Model

-- $setup
-- >>> import Data.List (sort)
--
-- a pixel coordinate is a pair of Int
type Coord = (Int, Int)

-- a pixel value is the transparency of its colour
-- (0.0 = transparent, 1.0 = opaque)
type Shade = Double

-- a raster pixel is represented by a coordinate and a shade
type Pixel = (Coord, Shade)

-- a raster is a list of pixels
type Raster = [Pixel]

coordToPoint :: Resolution -> Coord -> Point
coordToPoint z (x, y) = (x', y')
  where
    x' = fromIntegral x * z
    y' = fromIntegral y * z

pointToCoord :: Resolution -> Point -> Coord
pointToCoord z (x, y) = (x', y')
  where
    x' = round $ x / z
    y' = round $ y / z

-- | The 'updateView' function produces a rasterised picture from the model.
updateView :: Model -> Picture
updateView (Model ss t c z s) =
  translated 0 8 toolText & translated 0 7 colourText &
  (colourShapesToPicture z s ss) &
  coordinatePlane
  where
    colourText = stringToText (show c)
    toolText = stringToText toolString
    toolString =
      case t of
        PointTool _ -> "Point... click"
        LineTool _ -> "Line... click-drag-release"
        PolygonTool _ -> "Polygon... click vertices then spacebar"
        RectangleTool _ -> "Rectangle... click-drag-release"
        CircleTool _ -> "Circle... click-drag-release"
        EllipseTool _ -> "Ellipse... click-drag-release"
    stringToText = text . pack

rasterToPicture :: Colour -> Double -> Raster -> Picture
rasterToPicture (RGBA r g b _) z ps = pictures (map pixelToPicture ps)
  where
    pixelToPicture ((x, y), a) = translated x' y' p
      where
        p = coloured (RGBA r g b a) (solidRectangle z z)
        (x', y') = coordToPoint z (x, y)

colourShapesToPicture :: Resolution -> Smooth -> [ColourShape] -> Picture
colourShapesToPicture z s = foldr ((&) . (colourShapeToPicture z s)) blank

colourShapeToPicture :: Resolution -> Smooth -> ColourShape -> Picture
colourShapeToPicture z s (c, shape) =
  rasterToPicture (colourNameToColour c) z (shapeToRaster z s shape)

colourNameToColour :: ColourName -> Colour
colourNameToColour c =
  case c of
    Black -> black
    Red -> red
    Orange -> orange
    Yellow -> yellow
    Green -> green
    Blue -> blue
    Violet -> violet

-- | The 'shapeToRaster' function produces a raster for a shape at the given resolution (optionally smoothed).
-- It takes three arguments: a resolution, whether to smooth, and the shape.
shapeToRaster :: Resolution -> Smooth -> Shape -> Raster
shapeToRaster z False shape =
  case shape of
    Point p -> point (pointToCoord z p)
    Rectangle (x, y) w h -> rectangle (pointToCoord z a) (pointToCoord z b)
      where a = (x - w / 2, y - h / 2)
            b = (x + w / 2, y + h / 2)
    Line a b -> line (pointToCoord z a) (pointToCoord z b)
    Circle c r -> circle (pointToCoord z c) (round $ r / z)
    Ellipse c w h ->
      ellipse (pointToCoord z c) (round $ w / (2 * z)) (round $ h / (2 * z))
    Polygon ps -> polygon $ map (pointToCoord z) ps
shapeToRaster _ True _ = []

-- | The 'point' function produces a raster for a point.
-- Its argument is the raster coordinate of the point.
--
-- Examples:
--
-- >>> point (1,1)
-- [((1,1),1.0)]
point :: Coord -> Raster
point p = [(p, 1)]

-- | The 'rectangle' function produces a raster for a rectangle.
-- Its arguments are the raster coordinates of the opposing corners of the rectangle.
--
-- Examples:
--
-- prop> rectangle a a == point a
--
-- >>> sort $ rectangle (-1,-1) (1,1)
-- [((-1,-1),1.0),((-1,0),1.0),((-1,1),1.0),((0,-1),1.0),((0,1),1.0),((1,-1),1.0),((1,0),1.0),((1,1),1.0)]
rectangle :: Coord -> Coord -> Raster
rectangle = undefined -- TODO

-- | The 'line' function produces a raster for a line.
-- Its arguments are the raster coordinates of the end-points of the line.
--
-- Examples:
--
-- prop> line a a == point a
-- prop> a == (fst $ head $ line a b)
-- prop> b == (fst $ last $ line a b)
--
-- >>> sort $ line (-1,-1) (1,1)
-- [((-1,-1),1.0),((0,0),1.0),((1,1),1.0)]
line :: Coord -> Coord -> Raster
line = undefined -- TODO

-- | The 'polygon' function produces a raster for a closed polygon.
-- Its argument is the list of raster coordinates of the end-points of the lines.
--
-- Examples:
--
-- prop> polygon [a] == point a
-- prop> polygon [a,b] == line a b
--
-- >>> polygon []
-- []
--
-- >>> sort $ polygon [(0,0),(2,2)]
-- [((0,0),1.0),((1,1),1.0),((2,2),1.0)]
--
-- >>> sort $ polygon [(0,0),(2,2),(0,2)]
-- [((0,0),1.0),((0,1),1.0),((0,2),1.0),((1,1),1.0),((1,2),1.0),((2,2),1.0)]
polygon :: [Coord] -> Raster
polygon = undefined -- TODO

-- | The 'circle' function produces a raster for a circle.
-- Its arguments are the raster coordinates of its centre and its raster radius.
--
-- Examples:
--
-- prop> circle c 0 == point c
--
-- >>> sort $ circle (0,0) 2
-- [((-2,-1),1.0),((-2,0),1.0),((-2,1),1.0),((-1,-2),1.0),((-1,2),1.0),((0,-2),1.0),((0,2),1.0),((1,-2),1.0),((1,2),1.0),((2,-1),1.0),((2,0),1.0),((2,1),1.0)]
circle :: Coord -> Int -> Raster
circle = undefined -- TODO

-- | The 'ellipse' function produces a raster for an ellipse.
-- Its arguments are the raster coordinates of its centre and its
-- horizontal (semi-major) axis a, and vertical (semi-minor) axis b.
--
-- Examples:
--
-- prop> ellipse c 0 0 == point c
--
-- >>> sort $ ellipse (0,0) 2 1
-- [((-2,0),1.0),((-1,-1),1.0),((-1,1),1.0),((0,-1),1.0),((0,1),1.0),((1,-1),1.0),((1,1),1.0),((2,0),1.0)]
ellipse :: Coord -> Int -> Int -> Raster
ellipse = undefined -- TODO
