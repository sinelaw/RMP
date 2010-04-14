{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module System.RMP.USB where

import Foreign.C.Types
import Foreign 
import Foreign.ForeignPtrWrap

data RMPUSB
data RMPPacket

foreign import ccall unsafe "rmpusb.h rmpusb_new"
  c_RMPUSBNew :: IO (Ptr RMPUSB)

rmpUsbNew :: IO (Ptr RMPUSB)
rmpUsbNew = errorName "Failed to create RMP-USB context" . checkPtr $ c_RMPUSBNew
  
foreign import ccall unsafe "rmpusb.h rmpusb_delete"
  rmpUsbDelete :: Ptr RMPUSB -> IO ()

foreign import ccall unsafe "rmpusb.h rmpusb_write_packet"
  rmpUsbWritePacket :: Ptr RMPUSB -> Ptr RMPPacket -> IO CInt

foreign import ccall unsafe "rmpusb.h rmpusb_read_packet"
  rmpUsbReadPacket :: Ptr RMPUSB -> Ptr RMPPacket -> IO CInt


                       
foreign import ccall unsafe "rmpusb.h rmppacket_new"
  c_RMPPacketNew :: IO (Ptr RMPPacket)

rmpPacketNew :: IO (Ptr RMPPacket)
rmpPacketNew = errorName "Failed to create packet" . checkPtr $ c_RMPPacketNew
                    
foreign import ccall unsafe "rmpusb.h rmppacket_delete"
  rmpPacketDelete :: Ptr RMPPacket -> IO ()

foreign import ccall unsafe "rmpusb.h rmppacket_set_command_velocity"
  rmpPacketSetCommandVelocity :: Ptr RMPPacket -> CInt -> CInt -> IO ()



