module Main where

import Turtle
import Testing

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked.
tests :: [Test]
tests =
  [ testOnPolygon
  , testOnPolygonTwo
  , testOnTriangle
  , testOnComToState
  , testOnSierpinski
  , testOnStateToPic
  ]

-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your turtle interpreter, you can use this
-- space to say things like "driving out and back leaves you in the
-- same position".

-- | This function tests on polygon codes to check whether it is right or not
testOnPolygon :: Test
testOnPolygon = Test "decago will have 10 sides" (assertEqual (totalSide (polygon 10 9)) (10 ))

-- | This function tests on polygon codes to check whether it is right or not
testOnPolygonTwo :: Test
testOnPolygonTwo = Test label (assertApproxEqual (turtleDistance (polygon 10 5)) 50.0)
        where label = "The perimeter of decago with length 5 is 50"


-- | Because we are storing the turtle position in a pair of
-- 'Double's, addition behaves in ways you might not expect. The
-- 'assertApproxEqual' function tests that two 'Double's are close
-- enough to each other.

-- | This function tests on triangle codes to check whether it is right or not
testOnTriangle :: Test
testOnTriangle = Test label (assertApproxEqual (turtleDistance (triangle 5)) 15.0)
        where label = "The perimeter of triangle with length 5 is 15"


-- | 0.1 + 0.2 is NOT 0.3 when computed using 'Double's. This is why
-- we have provided you with the 'assertApproxEqual' function.

testOnStateToPic :: Test
testOnStateToPic = Test label (assertApproxEqual (penDownToDraw (comToState comp1100 initialState)) (actual))
        where label = "The total distance of the turtle draws comp1100 when pen is down "
              actual = turtleDistancePenDown comp1100

-- | You might find it easier to write tests against entire 'Point's.

-- | This function tests on ComToState codes to check whether it is right or not
testOnComToState :: Test
testOnComToState = Test "decago driving out and back leaves you in the same position"
  (assertPointApproxEqual ( lastPoint (comToState (polygon 10 9) initialState)) (0.0, 0.0))

-- | This test will fail, so you can see what a failing test looks
-- like.

-- | This function tests on Sierpinski codes to check whether it is right or not
testOnSierpinski :: Test
testOnSierpinski = Test str ((assertEqual (totalTriangle (sierpinski 10 10))) (19683))
        where str = "The total amount of smallest equilateral triangle in Sierpinski's Triangle with depth 10"

-- | This function find the last point of the turtle will stay
lastPoint :: [TurtleState] -> (Double,Double)
lastPoint s = case s of
    []-> (0,0)
    [Turtle p _ _ _] -> p
    _:xs -> lastPoint xs

-- | This function return total distance that turtle will travelled
turtleDistance :: [TurtleCommand] -> Double
turtleDistance tc = case tc of
    [] -> 0
    (Forward l):xs -> l+(turtleDistance xs)
    _:xs -> turtleDistance xs

-- | This function return the total sides of a triangle or polygon
totalSide :: [TurtleCommand] -> Int
totalSide tc = case tc of
    [] -> 0
    Turn _ : Forward _ : xs -> 1 + totalSide xs
    _ : xs-> totalSide xs

-- | This function will return the total amount of smallest equilateral triangle in Sierpinski's Triangle
totalTriangle :: [TurtleCommand] -> Int
totalTriangle tc = case tc of
    [] -> 0
    Forward _: Turn _:Forward _: Turn _:Forward _: Turn _ :xs -> 1 + totalTriangle xs
    _:xs -> totalTriangle xs

-- | Draw the distance of turtle passed when it is pen down
penDownToDraw :: [TurtleState] -> Double
penDownToDraw ts = case ts of
    [] -> 0
    Turtle (a,b) r (Forward d) MyPenDown : xs -> turtleDraw + penDownToDraw xs
            where turtleDraw = sqrt ((newX - a) ** 2+(newY - b) ** 2)
                  newX =(a+d*(cos r))
                  newY = (b+d*(sin r))
    _ : xs -> penDownToDraw xs

data CommandUpOrDown
  = Up TurtleCommand -- ^
  | Down TurtleCommand -- ^
  deriving (Eq, Show)


turtleDistancePenDown :: [TurtleCommand] -> Double
turtleDistancePenDown tc = sumComp (helper tc (Up PenUp))


helper :: [TurtleCommand] -> CommandUpOrDown -> [CommandUpOrDown]
helper tc cuod = case (cuod , tc) of
    (_ , []) -> []
    (Up _ ,PenDown:xs)  -> [Down PenDown] ++ helper xs (Down PenDown)
    (Up _ ,x:xs) -> helper xs (Up x)
    (Down _,PenUp:xs) ->  helper xs (Up PenUp)
    (Down _,x:xs) -> [Down x] ++ helper xs (Down x)


sumComp :: [CommandUpOrDown] -> Double
sumComp cl = case cl of
    [] -> 0
    Down (Forward d):xs -> d + sumComp xs
    _ :xs -> sumComp xs


-- | A haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests
