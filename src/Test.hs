module Main where

import System.RMP
import Control.Processor(runUntil, IOProcessor, trace)

import Control.Arrow


controller :: IOProcessor () (Int, Int)
controller = arr . const $ (0,-20)
  
main :: IO () 
main = runUntil (controller >>> trace >>> velocityRMP) () (const . return $ False) 
