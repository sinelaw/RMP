-- TODO: Check for null pointers and fail appropriately. perhaps use the ForeignPtrWrap module.

module System.RMP where

import System.RMP.USB

import Control.Processor(runUntil, IOSink, IOSource, IOProcessor, processor, wrapProcessor)

type Packet = Ptr RMPPacket

rmp :: IOProcessor Packet Packet -> IOProcessor () ()
rmp = wrapProcessor readPacket writePacket allocRMP readConv writeConv releaseRMP 
    where 
      readPacket :: () -> Ptr RMPUSB -> IO (Ptr RMPUSB)
      readPacket pkt rmpusb = do
        res <- c_RMPUSBReadPacket rmpusb pkt 
        -- todo: deal with res != 0
        return rmpusb
      
      writePacket :: Packet -> Ptr RMPUSB -> IO (Ptr RMPUSB)
      writePacket pkt rmpusb = do
        res <- c_RMPUSBWritePacket rmpusb pkt 
        -- todo: deal with res != 0
        return rmpusb
      
      allocRMP :: Packet -> IO (Ptr RMPUSB)
      -- todo: currently ignores the first packet, leave as is?
      allocRMP pkt = c_RMPUSBNew
      
      releaseRMP :: Ptr RMPUSB -> IO ()
      releaseRMP = c_RMPUSBDelete
      
      