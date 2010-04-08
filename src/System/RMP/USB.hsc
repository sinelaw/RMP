{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module System.RMP.USB where

import Foreign.C.Types
import Foreign 



data RMPUSB
data RMPPacket

foreign import ccall unsafe "rmpusb.h rmpusb_new"
  c_RMPUSBNew :: IO (Ptr RMPUSB)

foreign import ccall unsafe "rmpusb.h rmpusb_delete"
  c_RMPUSBDelete :: Ptr RMPUSB -> IO ()

foreign import ccall unsafe "rmpusb.h rmpusb_write_packet"
  c_RMPUSBWritePacket :: Ptr RMPUSB -> Ptr RMPPacket -> IO CInt

foreign import ccall unsafe "rmpusb.h rmpusb_read_packet"
  c_RMPUSBReadPacket :: Ptr RMPUSB -> Ptr RMPPacket -> IO CInt


                       
foreign import ccall unsafe "rmpusb.h rmppacket_new"
  c_RMPPacketNew :: IO (Ptr RMPPacket)
                    
foreign import ccall unsafe "rmpusb.h rmppacket_delete"
  c_RMPPacketNew :: Ptr RMPPacket -> IO ()

foreign import ccall unsafe "rmpusb.h rmppacket_set_command_velocity"
  c_RMPPacketNew :: Ptr RMPPacket -> CInt -> CInt -> IO ()
