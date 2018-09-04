module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "src/Controller.hs", "src/Model.hs", "src/View.hs"]
