module Main where

import System.RMP
import Control.Processor(runUntil, IOProcessor)

import Control.Arrow

controller :: IOProcessor Packet Packet
controller = arr (const (-20,0)) >>> velocityPacket
  
controlledRobot :: IOProcessor () ()
controlledRobot = rmp controller

main :: IO () 
main = runUntil controlledRobot () (const . return $ False) 
