-- TODO: Check for null pointers and fail appropriately. perhaps use the ForeignPtrWrap module.

module System.RMP 
    (Packet, rmp)
    where

import System.RMP.USB
import Foreign
import Control.Processor(IOProcessor, wrapProcessor)

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
      
      