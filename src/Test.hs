module Main where

import System.RMP
import Control.Processor(runUntil, IOProcessor)

import Control.Arrow


controller :: IOProcessor () (Int, Int)
controller = arr . const $ (-20,0)
  
controlledRobot :: IOProcessor () ()
controlledRobot = simpleRMP controller

main :: IO () 
main = runUntil controlledRobot () (const . return $ False) 
