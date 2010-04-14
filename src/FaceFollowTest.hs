module Main where

import qualified AI.CV.ImageProcessors as ImageProcessors
import AI.CV.ImageProcessors(ImageProcessor, ImageSource)
import AI.CV.OpenCV.CV as CV
import AI.CV.OpenCV.CxCore(CvRect(..), CvSize(..))
import AI.CV.OpenCV.Types(PImage)

import System.RMP(velocityRMP)

import Control.Processor(runUntil, IOProcessor)
import Data.VectorSpace(zeroV, (^-^), AdditiveGroup)

import Control.Monad(join)
import Control.Arrow


defaultHead :: a -> [a] -> a
defaultHead def [] = def
defaultHead _   xs = head xs

imageResizeTo :: Integral a => a -> a -> ImageProcessor
imageResizeTo resX resY = ImageProcessors.resize (fromIntegral resX) (fromIntegral resY) CV.CV_INTER_LINEAR

faceDetect :: IOProcessor PImage [CvRect]
faceDetect = ImageProcessors.haarDetect "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt.xml" 1.1 3 CV.cvHaarFlagNone (CvSize 20 20)

videoSource :: ImageSource
videoSource = ImageProcessors.camera 0

fromIntegral2 :: (Integral b, Num c) => (b, b) -> (c, c)
fromIntegral2 = join (***) fromIntegral

-- | Calculates the difference (direction) from the detect rect to the center of the screen.
-- the 'fromIntegral2' stuff is due to CInt not being a VectorSpace
calcDir :: (Integral a, Integral b, AdditiveGroup b) => a -> a -> CvRect -> (b, b)
calcDir resX resY rect = rectCenter ^-^ screenCenter
    where screenCenter = fromIntegral2 (resX `div` 2, resY `div` 2)
          rectCenter = fromIntegral2 (rectX rect + (rectWidth rect `div` 2), 
                                      rectY rect + (rectHeight rect `div` 2))
  
-- | Takes a direction vector (x,y) and returns required rotation speed to align with that direction.
-- for now we disregard the 'y' component, because we can't really point our robot "up" or "down" anyway.
dirToRotation :: (a,b) -> a
dirToRotation = fst
  
-- | calculates the (translation, rotation) pair used to control the robot, from a detected rect.
-- currently translation is constantly 0.
calcTransRot :: (Num c, Integral a, Integral b, AdditiveGroup b) => a -> a -> [CvRect] -> (c, b)
calcTransRot resX resY = const 0 &&& 
                         (defaultHead zeroV >>> calcDir resX resY >>> dirToRotation)

-- todo: a better solution than choosing the default if no faces detected, would be to keep tracking the last
-- known face?
controller :: IOProcessor PImage (Int, Int)
controller = imageResizeTo resX resY 
             >>> faceDetect 
             >>> arr (calcTransRot resX resY)
    where resX = 160
          resY = 120

main :: IO () 
main = runUntil (videoSource >>> controller >>> velocityRMP) () never
    where never = (const . return $ False) 
