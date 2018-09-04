--- Copyright 2018 The Australian National University, All rights reserved
module Main where

import CodeWorld (interactionOf)
import Controller (handleEvent, handleTime)
import Model (initialModel)
import View (updateView)

main :: IO ()
main = interactionOf initialModel handleTime handleEvent updateView
