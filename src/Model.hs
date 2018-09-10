--- Copyright 2018 The Australian National University, All rights reserved
module Model where

type Point = (Double, Double)

data Shape
  = Point Point
  | Line Point
         Point
  | Polygon [Point]
  | Rectangle Point -- centre
              Double -- width
              Double -- height
  | Circle Point -- centre
           Double -- radius
  | Ellipse Point -- centre
            Double -- width
            Double -- height
  deriving (Show)

type ColourShape = (ColourName, Shape)

type Resolution = Double

type Smooth = Bool

type Life = Bool

data Tool
  = PointTool (Maybe Point)
  | LineTool (Maybe Point)
  | PolygonTool [Point]
  | RectangleTool (Maybe Point)
  | CircleTool (Maybe Point)
  | EllipseTool (Maybe Point)
  deriving (Show)

data ColourName
  = Black
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  deriving (Show)

data Model =
  Model [ColourShape]
        Tool
        ColourName
        Resolution
  deriving (Show)

initialModel :: Model
initialModel = Model [] (PointTool Nothing) Black 1
