module Turtle where

import CodeWorld

type Radians = Double

-- | The commands that we can send to our turtle.
data TurtleCommand
  = Forward Double -- ^ Drive forward the given number of units,
                   -- drawing if the pen is down.
  | Turn Radians -- ^ Turn the turtle. Positive values are
                 -- anticlockwise; negative values are clockwise.
  | PenUp -- ^ Lift the pen, so that future 'Forward' instructions do
          -- not draw.
  | PenDown -- ^ Lower the pen, so that future 'Forward' instructions
            -- will draw.
  deriving (Eq, Show)

-- Task 1: Drawing Shapes

triangle :: Double -> [TurtleCommand]
triangle length =[Turn (-pi/6), PenDown ,Forward length ,Turn ((-2/3)*pi) ,Forward length ,Turn ((-2/3)*pi) ,Forward length ,Turn (-pi/2)]
      -- TODO

polygon :: Int -> Double -> [TurtleCommand]
polygon int length
      |int <= 2 = error "It just has 2 lengths ,which can not build a polygon" -- TODO
      |int == 3  = [PenDown,Turn (-(pi-polygonDegree)),Forward length,Turn (-(pi-polygonDegree)),Forward length ,Turn (-(pi-polygonDegree)) ,Forward length ,Turn (-(pi-polygonDegree)),PenUp]
      |int > 3 = [PenDown,Turn (-(pi-polygonDegree)) ,Forward length ,PenUp]++(polygon (int-1) length)
      where polygonDegree=(pi*(int-2))/int
-- Task 2: Interpreting Turtle Commands
data TurtleState
  =Turtle Point Radians TurtleCommand
  deriving (Eq, Show)

type InitialState = TurtleState

runTurtle :: [TurtleCommand] -> Picture
runTurtle = undefined -- TODO


-- Task 3: Sierpinski's Triangle
--   COMP1100: Implement this directly (Task 3A)
--   COMP1130: Implement this using an L-System (Task 3B)

sierpinski :: Int -> Double -> [TurtleCommand]
sierpinski = undefined -- TODO


-- Task 3B: L-Systems (COMP1130 Only)

lSystemCommands :: [TurtleCommand]
lSystemCommands = undefined -- TODO



-- | A more complex example to test your interpreter.
comp1100 :: [TurtleCommand]
comp1100 = concat [start, c, o, m, p, one, one, o, o]
  where
    start = [PenUp, f 1.5, l, f 9.25, r]
    c =
      [ r, f 1.5
      , l, f 0.5, PenDown
      , l, l', f d
      , r', f 1
      , r', f d
      , r', f 2
      , r', f d
      , r', f 1
      , r', f d, PenUp
      , r', f 2.5
      , l, f 1
      , l
      ]
    o =
      [ r, f 1.5
      , l, f 0.5, PenDown
      , l, l', f d
      , r', f 1
      , r', f d
      , r', f 2
      , r', f d
      , r', f 1
      , r', f d
      , r', f 2, PenUp
      , f 0.5
      , l, f 1
      , l
      ]
    m =
      [ l, f 0.5, r, PenDown
      , f 3
      , r, r', f (d * 2)
      , l, f (d * 2)
      , r, r', f 3, PenUp
      , l, f 1, l
      ]
    p =
      [ l, f 0.5, r, PenDown
      , f 2.5
      , r', f d
      , r', f 1
      , r', f d
      , r', f 1
      , r', f d
      , r', f 1
      , r', f d, PenUp
      , r, r', f 3
      , r, f 1.5
      , l, l
      ]
    one =
      [ PenDown
      , r, f 1
      , l, l, f 0.5
      , r, f 3
      , l, l', f d, PenUp
      , f d
      , l', f 2
      , l, f 2.5
      , l
      ]

    f = Forward

    -- Left/Right turns, 90 degrees. Primed versions (the ones with an
    -- ' after: l', r') are 45 degrees.
    l = Turn (pi / 2)
    l' = Turn (pi / 4)
    r = Turn (-pi / 2)
    r' = Turn (-pi / 4)

    -- Diagonal length of a right-angle triangle with both sides 0.5
    d = sqrt (2 * 0.5 * 0.5)
