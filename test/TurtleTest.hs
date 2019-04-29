module Main where

import Turtle
import Testing

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked.
tests :: [Test]
tests =
  [ exampleTestOne
  , exampleTestTwo
  , exampleTestThree
  , exampleTestFour
  , exampleFailure
  , exampleTestFive
  ]

-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your turtle interpreter, you can use this
-- space to say things like "driving out and back leaves you in the
-- same position".
exampleTestOne :: Test
exampleTestOne = Test "decago will have 10 sides" (assertEqual (totalSide (polygon 10 9)) (10 ))


-- | Because we are storing the turtle position in a pair of
-- 'Double's, addition behaves in ways you might not expect. The
-- 'assertApproxEqual' function tests that two 'Double's are close
-- enough to each other.
exampleTestTwo :: Test
exampleTestTwo = Test label (assertApproxEqual (turtleDistance (triangle 5)) 15.0)
        where label = "The perimeter of triangle with length 5 is 15"


-- | 0.1 + 0.2 is NOT 0.3 when computed using 'Double's. This is why
-- we have provided you with the 'assertApproxEqual' function.
exampleTestThree :: Test
exampleTestThree = Test "0.1 + 0.2 /= 0.3" (assertNotEqual (0.1 + 0.2) (0.3 :: Double))

-- | You might find it easier to write tests against entire 'Point's.
exampleTestFour :: Test
exampleTestFour = Test "decago driving out and back leaves you in the same position"
  (assertPointApproxEqual ( lastPoint (comToState (polygon 10 9) initialState)) (0.0, 0.0))

exampleTestFive :: Test
exampleTestFive = Test label (assertApproxEqual (turtleDistance (polygon 10 5)) 50.0)
        where label = "The perimeter of decago with length 5 is 50"

-- | This test will fail, so you can see what a failing test looks
-- like.
exampleFailure :: Test
exampleFailure = Test str ((assertEqual (totalTriangle (sierpinski 10 10))) (19683))
        where str = "The total amount of smallest equilateral triangle in Sierpinski's Triangle with depth 10"


lastPoint :: [TurtleState] -> (Double,Double)
lastPoint s = case s of
    []-> (0,0)
    [Turtle p _ _ _] -> p
    _:xs -> lastPoint xs

turtleDistance :: [TurtleCommand] -> Double
turtleDistance tc = case tc of
    [] -> 0
    (Forward l):xs -> l+(turtleDistance xs)
    _:xs -> turtleDistance xs

totalSide :: [TurtleCommand] -> Int
totalSide tc = case tc of
    [] -> 0
    Turn _ : Forward _ : xs -> 1 + totalSide xs
    _ : xs-> totalSide xs

totalTriangle :: [TurtleCommand] -> Int
totalTriangle tc = case tc of
    [] -> 0
    Forward _: Turn _:Forward _: Turn _:Forward _: Turn _ :xs -> 1 + totalTriangle xs
    _:xs -> totalTriangle xs
-- | A haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests
