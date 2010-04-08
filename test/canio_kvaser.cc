
#include "canio_kvaser.h"

#define CANIO_KVASER_CHANNEL 1

CANIOKvaser::CANIOKvaser() : CANIO(), can_handle(-1)
{
}

CANIOKvaser::~CANIOKvaser()
{
}

/* Initializes the class by opening the can channel
 *
 * returns: 0 on success, negative on error
 */
int
CANIOKvaser::Init()
{
  int ret;

    if((can_handle = canOpenChannel(CANIO_KVASER_CHANNEL,
                                    canWANT_EXCLUSIVE |
                                    canWANT_EXTENDED)) < 0) {
        return can_handle;
    }
    
    // set the channel params: 500Kbps ... CANLIB will set the other params
    // to defaults if we use BAUD_500K
    if ((ret = canSetBusParams(can_handle, 
                               BAUD_500K, 0, 0, 0, 0, 0)) < 0) {
      return ret;
    }
    
    // set filter to only accept packets we are interested in...
    // that is, messages with IDs 0x400, 0x401, ..., 0x407
    if ((ret = canAccept(can_handle,
                         0x400, canFILTER_SET_MASK_STD)) < 0) {
      return ret;
    }

    if ((ret = canAccept(can_handle,
                         0x400, canFILTER_SET_CODE_STD)) < 0) {
      return ret;
    }

    // turn on the bus!
    if ((ret = canBusOn(can_handle)) < 0) {
      return ret;
    }
  
  return 0;
}
  
/* Closes the CAN channel
 *
 * returns: 0 on success, negative otherwise
 */
int
CANIOKvaser::Shutdown()
{
  int ret;
  if (can_handle >= 0) {
      if ((ret = canClose(can_handle)) < 0) {
          return ret;
      }
      can_handle = -1;  // mark as invalid
  }
  return 0;
}
  
/* Writes the given packet
 *
 * returns: 0 on success, negative error code otherwise
 */
int
CANIOKvaser::WritePacket(CanPacket &pkt)
{
  int ret;

  //printf("CANIO: WRITE: pkt: %s\n", pkt.toString());

  if ((ret = canWriteWait(can_handle, pkt.id, pkt.msg, pkt.dlc, 
                          pkt.flags, 1000)) < 0) {
      printf("CANIO: write wait error %d\n", ret);
      return ret;
  }

  
  if ((ret = canWriteSync(can_handle, 10000)) < 0) {
      printf("CANIO: error %d on write sync\n", ret);
      switch (ret) {
      case canERR_TIMEOUT:
          printf("CANIO: TIMEOUT error\n");
          break;
      default:
          break;
      }
      return ret;
  }
    
  return 0;
}

/* Reads a packet and writes it into msg (must already be allocated)
 * Sets id, dlc, and flags
 *
 * returns: # bytes in msg if a packet is read, 0 if no packet available, and
 * negative on error
 */
int
CANIOKvaser::ReadPacket(CanPacket *pkt)
{
  int ret=0;
  long unsigned time;

  if((ret = canReadWait(can_handle, &(pkt->id), &(pkt->msg), 
		     &(pkt->dlc), &(pkt->flags), &time, -1)) < 0) 
  {
    // either no messages or an error
    if(ret == canERR_NOMSG)
      return 0;
    else
      return ret;
  }

  return pkt->dlc;
}


