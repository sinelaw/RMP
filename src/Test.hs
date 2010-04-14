module Main where

import System.RMP
import Control.Processor(runUntil, IOProcessor)

import Control.Arrow


controller :: IOProcessor () (Int, Int)
controller = arr . const $ (-20,0)
  
main :: IO () 
main = runUntil (controller >>> velocityRMP) () (const . return $ False) 
