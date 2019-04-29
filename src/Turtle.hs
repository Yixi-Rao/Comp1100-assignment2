{-|
Module      : <Turtle>
Description : <Turtle draws picture according to commands>
Maintainer  : <u6826541@anu.edu.an>
Name        : <Yixi Rao>
Assignment  : <2>

<Turtle draws picture according to commands.>

-}
module Turtle where

import CodeWorld hiding (polygon)

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
-- | It is a list of turtle commands , which will draw a triangle
triangle :: Double -> [TurtleCommand]
triangle l =[Turn ((-2/3)*pi), Forward l ,Turn ((-2/3)*pi) ,Forward l ,Turn ((-2/3)*pi) ,Forward l ]
      -- TODO
-- | It will generate a list of turtle commands , which will draw a polygon with n sides and len length
polygon :: Int -> Double -> [TurtleCommand]
polygon n len = helpPolygon n n len

-- | A helper function,which will help to build a polygon with an another n sides input as to help to define the angle
helpPolygon:: Int -> Int -> Double -> [TurtleCommand]
helpPolygon d m s
    |m == 3 = [PenDown,Turn angle,Forward s,Turn angle,Forward s,Turn angle,Forward s,PenUp]
    |otherwise = [PenDown,Turn angle,Forward s,PenUp]++(helpPolygon d (m-1) s)
    where angle= (2*pi)/(fromIntegral d)

-- Task 2: Interpreting Turtle Commands

-- | Pen is up or down
data UqandDown
  = MyPenUp -- ^ Pen is up , which will move to a new point without drawing anything
  | MyPenDown -- ^ Pen is down and draw a new line according to two points
  deriving (Eq, Show)

-- | All The information about the turtle
data TurtleState
  =Turtle Point Radians TurtleCommand UqandDown -- ^ the facing and position and the command that will performed
                                                    -- or not according to the pen is up or down
  deriving (Eq, Show)

-- | the initial state of the turtle
initialState :: TurtleState
initialState = Turtle (0,0) (pi/2) PenDown MyPenDown

-- | run all commands and draw a picture
runTurtle :: [TurtleCommand] -> Picture
runTurtle command= stateToPic (comToState command initialState)


-- | Transform the commands to the list of TurtleStates
comToState :: [TurtleCommand]-> TurtleState-> [TurtleState]
comToState l (Turtle (a,b) ir tc ud) = case l of
    [] -> []
    PenDown : xs -> [Turtle (a,b) ir PenDown MyPenDown]++(comToState xs (Turtle (a,b) ir tc MyPenDown))
    PenUp : xs -> [Turtle (a,b) ir PenUp MyPenUp] ++ (comToState xs (Turtle (a,b) ir tc MyPenUp))
    Forward d : xs->[Turtle (a,b) ir (Forward d) ud]++ (comToState xs (Turtle (newX,newY) ir (Forward d) ud))
            where newX = (a+d*(cos ir))
                  newY = (b+d*(sin ir))
    Turn r : xs -> [Turtle (a,b) (nr) (Turn (nr)) ud] ++ (comToState xs (Turtle (a,b) (nr) (Turn (nr)) ud))
            where nr = r+ir


-- | This function turns all the states to picture according to the pen's situation
stateToPic :: [TurtleState] -> Picture
stateToPic list = case list of
    [] -> coordinatePlane
    Turtle (a,b) r (Forward d) MyPenDown : xs -> ( polyline [(a,b),(newX,newY)])& (stateToPic xs)
            where newX = (a+d*(cos r))
                  newY = (b+d*(sin r))
    Turtle (a,b) r (Forward d) MyPenUp : xs -> ( polyline [(newX,newY),(newX,newY)])& (stateToPic xs)
            where newX = (a+d*(cos r))
                  newY = (b+d*(sin r))
    _ :xs-> stateToPic xs



-- Task 3: Sierpinski's Triangle
--   COMP1100: Implement this directly (Task 3A)
--   COMP1130: Implement this using an L-System (Task 3B)

-- | Generate a list of commands to draw a sierpinski triangle
sierpinski :: Int -> Double -> [TurtleCommand]
sierpinski n l = draw n n l

-- | This helper function will generate a list, which will draw the sierpinski triangle
draw:: Int ->Int-> Double -> [TurtleCommand]
draw n k l
    | n == 1 = [Forward len,Turn (pi*2/3),Forward len,Turn (pi*2/3),Forward len,Turn (pi*2/3)]
    | otherwise = draw (n-1) k l ++ [f] ++ draw (n-1) k l ++ [tp ,f,tn] ++ draw (n-1) k l ++ [tn ,f ,tp]
            where len = l / (2 ** (fromIntegral (k-1)))
                  move = l / (2 ** (fromIntegral (k-(n-1))))
                  tp = Turn (pi*2/3)
                  tn = Turn (-pi*2/3)
                  f = Forward move
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

