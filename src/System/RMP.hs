-- TODO: Check for null pointers and fail appropriately. perhaps use the ForeignPtrWrap module.

module System.RMP where

import System.RMP.USB
import Foreign
import Foreign.C.Types
import Control.Processor(IOProcessor, wrapProcessor, processor)

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

velocityPacket :: IOProcessor (CInt, CInt) Packet
velocityPacket = processor proc alloc conv release 
    where
      proc :: (CInt,CInt) -> Packet -> IO Packet
      proc (trans, rot) pkt = do
        rmpPacketSetCommandVelocity pkt trans rot 
        return pkt
        
      alloc :: (CInt,CInt) -> IO Packet
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
              