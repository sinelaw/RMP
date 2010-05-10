-- A face-following robot
-- Todo: improve following by taking note of how much we moved

module Main where

import qualified AI.CV.ImageProcessors as ImageProcessors
import AI.CV.ImageProcessors(ImageProcessor, ImageSource, runTillKeyPressed)
import AI.CV.OpenCV.CV as CV
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))
import AI.CV.OpenCV.Types(PImage)

import System.RMP(velocityRMP)

import Control.Processor(runUntil, IOProcessor, (--<), trace, fir, headOrLast)
import Data.VectorSpace(zeroV, (^-^), (^*), AdditiveGroup)

import Control.Monad(join)
import Prelude hiding ((.),id)
import Control.Arrow
import Control.Category

import qualified Debug.Trace as DT
traceId x = DT.trace (show x) x

defaultHead :: a -> [a] -> a
defaultHead def [] = def
defaultHead _   xs = head xs

imageResizeTo :: Integral a => a -> a -> ImageProcessor
imageResizeTo resX resY = ImageProcessors.resize (fromIntegral resX) (fromIntegral resY) CV.CV_INTER_LINEAR

faceDetect :: IOProcessor PImage [CvRect]
faceDetect = ImageProcessors.haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt_tree.xml" 1.1 3 CV.cvHaarFlagNone (CvSize 20 20)

videoSource :: ImageSource
videoSource = ImageProcessors.camera 0

fromIntegral2 :: (Integral b, Num c) => (b, b) -> (c, c)
fromIntegral2 = join (***) fromIntegral


absMax b a = max (min a (abs b)) (- (abs b))
-----------------------------------------------------------------------------

calcDist :: (Num x, Ord x) => x -> CvRect -> x
calcDist reference rect = if rectArea > 1 then reference - rectArea else 0
    where w = rectWidth  rect
          h = rectHeight rect
          rectArea = traceId . uncurry (*) $ fromIntegral2 (w,h)

-- | Calculates the difference (direction) from the detect rect to the center of the screen.
-- the 'fromIntegral2' stuff is due to CInt not being a VectorSpace
calcDir :: (Integral a, Integral b, AdditiveGroup b) => a -> a -> CvRect -> (b, b)
calcDir resX resY rect = if rect /= zeroV then rectCenter ^-^ screenCenter else (0,0)
    where screenCenter = fromIntegral2 (resX `div` 2, resY `div` 2)
          rectCenter = fromIntegral2 (rectX rect + (rectWidth rect `div` 2), 
                                      rectY rect + (rectHeight rect `div` 2))
  
-- | Takes a direction vector (x,y) and returns required rotation speed to align with that direction.
-- for now we disregard the 'x' component, because we can't really point our robot "up" or "down" anyway.
dirToRotation :: (Num a, Ord a, Num b, Ord b) => (a,b) -> a
dirToRotation (yRot, xRot) = - (absMax 200 (yRot*4) )
  
-- | calculates the (translation, rotation) pair used to control the robot, from a detected rect.
-- currently translation is constantly 0.
calcTransRot :: (Num c, Ord c, Integral c, Integral a, Integral b, AdditiveGroup b) => a -> a -> CvRect -> (c, b)
calcTransRot resX resY = (absMax 35 . (flip div 10) . calcDist (fromIntegral ((resX*resY) `div` 15)))  &&& 
                         (calcDir resX resY >>> dirToRotation)

-- todo: a better solution than choosing the default if no faces detected, would be to keep tracking the last
-- known face?
controller :: Integral a => a -> a -> IOProcessor CvRect (Int, Int)
controller resX resY =  arr (calcTransRot resX resY)

clock :: IO Double
clock = return 1 -- todo implement really in some module that wraps SDL, GLUT or whatever.

main :: IO ()  
main = runTillKeyPressed (videoSource >>> imageResizeTo resX resY 
             >>> (id &&& averageFace) >>> ((second (faceToVel >>> trace >>> velocityRMP)) &&& showVideo))
      where showVideo = (second . arr $ return) >>> ImageProcessors.drawRects >>> ImageProcessors.window 0
            averageFace = fir [0.9,0.1] 1 clock (headOrLast zeroV clock faceDetect)
            resX = 160
            resY = 120
            faceToVel = controller resX resY



