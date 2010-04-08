#ifndef _KVASER_CANLIB_
#define _KVASER_CANLIB_

#include <canlib.h>
#include "canio.h"

class CANIOKvaser : public CANIO
{
  private:
    canHandle can_handle;
    
  public:
    CANIOKvaser();
    virtual ~CANIOKvaser();
    virtual int Init();
    virtual int ReadPacket(CanPacket *pkt);
    virtual int WritePacket(CanPacket &pkt);
    virtual int Shutdown();
};

#endif
