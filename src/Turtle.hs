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

triangle :: Double -> [TurtleCommand]
triangle l =[Turn ((-2/3)*pi), Forward l ,Turn ((-2/3)*pi) ,Forward l ,Turn ((-2/3)*pi) ,Forward l ]
      -- TODO

polygon :: Int -> Double -> [TurtleCommand]
polygon n len = helpdegree n n len


helpdegree:: Int -> Int -> Double -> [TurtleCommand]
helpdegree d m s
    |m == 3 = [PenDown,Turn angle,Forward s,Turn angle,Forward s,Turn angle,Forward s,PenUp]
    |m > 3 = [PenDown,Turn angle,Forward s,PenUp]++(helpdegree d (m-1) s)
    |otherwise = error "not a number"
    where angle= (2*pi)/(fromIntegral d)
-- Task 2: Interpreting Turtle Commands
data UqandDown =    MyPenUp | MyPenDown
  deriving (Eq, Show)


data TurtleState
  =Turtle Point Radians TurtleCommand UqandDown
  deriving (Eq, Show)

initialState :: TurtleState
initialState = Turtle (0,0) (pi/2) PenDown MyPenDown

runTurtle :: [TurtleCommand] -> Picture
runTurtle command= dontknow (firstdont command initialState)

         -- TODO
firstdont :: [TurtleCommand]-> TurtleState-> [TurtleState]
firstdont l (Turtle (a,b) ir tc ud) = case l of
            x:xs ->case x of
                PenDown -> [Turtle (a,b) ir PenDown MyPenDown]++(firstdont xs (Turtle (a,b) ir tc MyPenDown))
                PenUp -> [Turtle (a,b) ir PenUp MyPenUp] ++ (firstdont xs (Turtle (a,b) ir tc MyPenUp))
                Forward d ->[Turtle (a,b) ir (Forward d) ud]++ (firstdont xs (Turtle ((a+d*(cos ir)),(b+d*(sin ir))) ir (Forward d) ud))
                Turn r -> [Turtle (a,b) (r+ir) (Turn (r+ir)) ud] ++ (firstdont xs (Turtle (a,b) (r+ir) (Turn (r+ir)) ud))
            _-> []

dontknow :: [TurtleState] -> Picture
dontknow list = case list of
            x:xs -> case x of
                Turtle (a,b) r (Forward d) ud->case ud of
                    MyPenDown -> ( polyline [(a,b),((a+d*(cos r)),(b+d*(sin r)))])& (dontknow xs)
                    MyPenUp -> ( polyline [((a+d*(cos r)),(b+d*(sin r))),((a+d*(cos r)),(b+d*(sin r)))])& (dontknow xs)
                _ -> dontknow xs
            [] -> coordinatePlane



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
