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
  ]

-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your turtle interpreter, you can use this
-- space to say things like "driving out and back leaves you in the
-- same position".
exampleTestOne :: Test
exampleTestOne = Test "2 + 2 == 4" (assertEqual (2 + 2) (4 :: Int))

-- | Because we are storing the turtle position in a pair of
-- 'Double's, addition behaves in ways you might not expect. The
-- 'assertApproxEqual' function tests that two 'Double's are close
-- enough to each other.
exampleTestTwo :: Test
exampleTestTwo = Test "0.1 + 0.2 =~ 0.3" (assertApproxEqual (0.1 + 0.2) 0.3)

-- | 0.1 + 0.2 is NOT 0.3 when computed using 'Double's. This is why
-- we have provided you with the 'assertApproxEqual' function.
exampleTestThree :: Test
exampleTestThree = Test "0.1 + 0.2 /= 0.3" (assertNotEqual (0.1 + 0.2) (0.3 :: Double))

-- | You might find it easier to write tests against entire 'Point's.
exampleTestFour :: Test
exampleTestFour = Test "(0.0, 0.1 + 0.2) =~ (0.0, 0.3)"
  (assertPointApproxEqual (0.0, 0.1 + 0.2) (0.0, 0.3))

-- | This test will fail, so you can see what a failing test looks
-- like.
exampleFailure :: Test
exampleFailure = Test "0.1 + 0.2 == 0.3" (assertEqual (0.1 + 0.2) (0.3 :: Double))

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests
