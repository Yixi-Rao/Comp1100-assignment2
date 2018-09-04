--- Copyright 2018 The Australian National University, All rights reserved
module Controller where

import CodeWorld hiding (Point, circle, rectangle)
import Model

import Data.Char (isDigit)
import Data.Text (pack, unpack)

handleTime :: Double -> Model -> Model
handleTime = flip const

handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss t c z s) =
  case event of
    KeyPress key
      | k == "Esc" -> initialModel
        -- revert to an empty canvas
      | k == "D" -> trace (pack (show m)) m
        --   the current model on the console
      | k == "Backspace" || k == "Delete" -> Model (drop 1 ss) t c z s
        -- drop the last added shape
      | k == "S" -> Model ss t c z (not s)
        -- turn smoothing on/off
      | k == "0" -> Model ss t c (1 / 10) s
        -- set the pixel resolution to 0.1
      | isDigit . head $ k -> Model ss t c (1 / read [head k]) s
        -- set the pixel resolution to 1/k
      | k == "-" || k == "," -> Model ss t c (z / 2.0) s
        -- halve the pixel resolution
      | k == "=" || k == "." -> Model ss t c (z * 2.0) s
        -- double the pixel resolution
      | k == "T" -> maybe m (\t' -> Model ss t' c z s) (switchTool t)
      | k == "C" -> Model ss t (switchColour c) z s
      | k == " " ->
        case t of
          PolygonTool [] -> m
          PolygonTool ps ->
            Model ((c, Polygon ps) : (drop 1 ss)) (PolygonTool []) c z s
          _ -> m
      | otherwise -> m
      where k = unpack key
    PointerPress p ->
      maybe m (\(s', t') -> Model ((c, s') : ss) t' c z s) (useTool t p)
    PointerMovement p ->
      maybe
        m
        (\(s', t') -> Model ((c, s') : (drop 1 ss)) t' c z s)
        (moveTool t p)
    PointerRelease p ->
      maybe
        m
        (\(s', t') -> Model ((c, s') : (drop 1 ss)) t' c z s)
        (applyTool t p)
    _ -> m

switchTool :: Tool -> Maybe Tool
switchTool t =
  case t of
    PointTool Nothing -> Just (LineTool Nothing)
    LineTool Nothing -> Just (PolygonTool [])
    PolygonTool [] -> Just (RectangleTool Nothing)
    RectangleTool Nothing -> Just (CircleTool Nothing)
    CircleTool Nothing -> Just (EllipseTool Nothing)
    EllipseTool Nothing -> Just (PointTool Nothing)
    _ -> Nothing

switchColour :: ColourName -> ColourName
switchColour c =
  case c of
    Black -> Red
    Red -> Orange
    Orange -> Yellow
    Yellow -> Green
    Green -> Blue
    Blue -> Violet
    Violet -> Black

useTool :: Tool -> Point -> Maybe (Shape, Tool)
useTool t p =
  case t of
    PointTool Nothing -> Just (Point p, PointTool (Just p))
    LineTool Nothing -> Just (Line p p, LineTool (Just p))
    PolygonTool [] -> Just (Polygon [p], t)
    RectangleTool Nothing -> Just (rectangle p p, RectangleTool (Just p))
    CircleTool Nothing -> Just (circle p p, CircleTool (Just p))
    EllipseTool Nothing -> Just (ellipse p p, EllipseTool (Just p))
    _ -> Nothing

moveTool :: Tool -> Point -> Maybe (Shape, Tool)
moveTool t p1 =
  case t of
    PointTool (Just _) -> Just (Point p1, PointTool (Just p1))
    LineTool (Just p0) -> Just (Line p0 p1, t)
    PolygonTool [] -> Nothing
    PolygonTool ps -> Just (Polygon (p1 : ps), t)
    RectangleTool (Just p0) -> Just (rectangle p0 p1, t)
    CircleTool (Just p0) -> Just (circle p0 p1, t)
    EllipseTool (Just p0) -> Just (ellipse p0 p1, t)
    _ -> Nothing

applyTool :: Tool -> Point -> Maybe (Shape, Tool)
applyTool t p1 =
  case t of
    PointTool (Just p0) -> Just ((Point p0), PointTool Nothing)
    LineTool (Just p0) -> Just ((Line p0 p1), LineTool Nothing)
    PolygonTool ps -> Just (Polygon (p1 : ps), PolygonTool (p1 : ps))
    RectangleTool (Just p0) -> Just (rectangle p0 p1, RectangleTool Nothing)
    CircleTool (Just p0) -> Just (circle p0 p1, CircleTool Nothing)
    EllipseTool (Just p0) -> Just (ellipse p0 p1, EllipseTool Nothing)
    _ -> Nothing

rectangle :: Point -> Point -> Shape
rectangle (x0, y0) (x1, y1) = Rectangle c w h
  where
    c = ((x0 + x1) / 2, (y0 + y1) / 2)
    w = abs (x1 - x0)
    h = abs (y1 - y0)

circle :: Point -> Point -> Shape
circle c@(x0, y0) (x1, y1) = Circle c r
  where
    r = sqrt (dx * dx + dy * dy)
    dx = abs (x1 - x0)
    dy = abs (y1 - y0)

ellipse :: Point -> Point -> Shape
ellipse (x0, y0) (x1, y1) = Ellipse c w h
  where
    c = ((x0 + x1) / 2, (y0 + y1) / 2)
    w = abs (x1 - x0)
    h = abs (y1 - y0)
