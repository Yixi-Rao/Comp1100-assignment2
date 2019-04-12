{-# LANGUAGE OverloadedStrings #-}

module Main where

import CodeWorld
import qualified Turtle as Turtle

data Mode
  = Triangle
  | Polygon Int
  | Comp1100
  | Sierpinski
  | LSystem

main :: IO ()
main = interactionOf Triangle unchanging handleEvent render

unchanging :: Double -> Mode -> Mode
unchanging _ mode = mode

handleEvent :: Event -> Mode -> Mode
handleEvent (KeyPress k) mode
  | k == "C" = Comp1100
  | k == "L" = LSystem
  | k == "S" = Sierpinski
  | k == "T" = Triangle
  | k == "P" = case mode of
      Polygon _ -> mode
      _ -> Polygon 5
  | k == "-" = case mode of
      Polygon n -> Polygon (max (pred n) 3)
      _ -> mode
  | k == "=" = case mode of
      Polygon n -> Polygon (succ n)
      _ -> mode
handleEvent _ mode = mode

render :: Mode -> Picture
render mode = picture & coordinatePlane
  where picture = Turtle.runTurtle
          (case mode of
             Triangle -> Turtle.triangle 6.5
             Polygon n -> Turtle.polygon n 3.0
             Comp1100 -> Turtle.comp1100
             Sierpinski -> Turtle.sierpinski 5 10.0
             LSystem -> Turtle.lSystemCommands)
