-- TODO: Check for null pointers and fail appropriately. perhaps use the ForeignPtrWrap module.

module System.RMP where

import System.RMP.USB
import Foreign
import Foreign.C.Types
import Control.Processor(IOProcessor, wrapProcessor, processor)
import Control.Arrow((>>>),arr)

type Packet = Ptr RMPPacket

data RMPState = RMPState { rmpCtx :: Ptr RMPUSB, rmpReadPkt :: Packet }

rmp :: IOProcessor Packet Packet -> IOProcessor () ()
rmp = wrapProcessor readPacket writePacket allocRMP readConv writeConv releaseRMP 
    where 
      readPacket :: () -> RMPState -> IO RMPState
      readPacket _ state = do
        res <- rmpUsbReadPacket (rmpCtx state) (rmpReadPkt state)
        -- todo: deal with res != 0
        return state
      
      readConv :: RMPState -> IO Packet
      readConv = return . rmpReadPkt
      
      writePacket :: Packet -> RMPState -> IO RMPState
      writePacket pkt state = do
        res <- rmpUsbWritePacket (rmpCtx state) pkt
        -- todo: deal with res != 0
        return state
      
      writeConv :: RMPState -> IO ()
      writeConv = const (return ())
      
      allocRMP :: () -> IO RMPState
      allocRMP _ = do
        ctx <- rmpUsbNew
        rPkt <- rmpPacketNew
        return (RMPState ctx rPkt)
      
      releaseRMP :: RMPState -> IO ()
      releaseRMP state = do
        rmpPacketDelete (rmpReadPkt state)
        rmpUsbDelete (rmpCtx state)

-----------------------------------------------------------------------------

velocityPacket :: Integral a => IOProcessor (a, a) Packet
velocityPacket = processor proc alloc conv release 
    where
      proc :: Integral a => (a,a) -> Packet -> IO Packet
      proc (trans, rot) pkt = do
        rmpPacketSetCommandVelocity pkt (fromIntegral trans) (fromIntegral rot)
        return pkt
        
      alloc :: Integral a => (a,a) -> IO Packet
      alloc _ = do
        pkt <- rmpPacketNew
        return pkt

      conv :: Packet -> IO Packet
      conv = return
      
      release :: Packet -> IO ()
      release pkt = do
        rmpPacketDelete pkt
      


-- data Packet = Empty 
--             | PitchRoll { pitchAngle :: Double; pitchRate :: Double, rollAngle :: Double, rollRate :: Double }
--             | VelocityYaw { wheelsVelocity :: (Double, Double), yawRate :: Double }
--             | WheelDisplacement { wheelsDisplacement :: (Double, Double) }

      
-----------------------------------------------------------------------------

-- A dummy version of rmp that:
-- 1. Ignores all robot telemetries
-- 2. Can only send velocity commands
simpleRMP :: Integral a => IOProcessor () (a, a) -> IOProcessor () ()
simpleRMP controller = rmp (ignoreAll >>> controller >>> velocityPacket)
    where ignoreAll = arr . const $ ()

