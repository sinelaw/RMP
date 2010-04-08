#include "canio_kvaser.h"
#include <stdio.h>


int main(void) {
    CanPacket packet;
    CANIOKvaser can;
    can.Init();
    can.ReadPacket(&packet);
    can.Shutdown();
}
