module Main where

import Control.Concurrent (threadDelay)
import State
import Draw

-- Assuming speed = 1 block per second for version 1.
-- Refresh rate of 1 second.
--------------------------------------------------------------------------------
-- Animate!

-- Homework: before printing, let's clear the terminal.
-- Use hoogle.haskell.org or search google to check how to clear the terminal.
animate :: Env -> State -> IO ()
animate env state = putStr (drawState env state) >> threadDelay 1000000 >> animate env (next env state)

main = animate (Env (11, 5)) (State (1,1) (1,1))
