/* C++ -> C Interface */

#include <unistd.h>
#include "canio_rmpusb.h"
#include "rmpusb.h"

struct rmpusb {
    CANIOrmpusb *canio;
};

struct rmppacket {
    CanPacket pkt;
};

rmpusb_t *rmpusb_new()
{
    rmpusb_t *res = new rmpusb_t;
    if (NULL == res) {
        return NULL;
    }
    
    res->canio = new CANIOrmpusb;
    if (NULL == res->canio) {
        goto ERROR_EXIT;
    }

    if (0 != res->canio->Init()) {
        goto ERROR_EXIT;
    }

    return res;

ERROR_EXIT:
    delete res;
    return NULL;
    
}

void rmpusb_delete(rmpusb_t *rmpusb)
{
    if (NULL == rmpusb) {
        return;
    }
    if (NULL != rmpusb->canio) {
        rmpusb->canio->Shutdown();
        delete rmpusb->canio;
    }
    delete rmpusb;
}

int rmpusb_write_packet(rmpusb_t *rmpusb, rmppacket_t *cnpkt)
{
    return rmpusb->canio->WritePacket(cnpkt->pkt);
}

int rmpusb_read_packet(rmpusb_t *rmpusb, rmppacket_t *cnpkt)
{
    return rmpusb->canio->ReadPacket(&(cnpkt->pkt));
}

/******************************************************************************/

rmppacket_t *rmppacket_new() {
    return new rmppacket_t;
}

void rmppacket_delete(rmppacket_t *pkt) {
    delete pkt;
}


void rmppacket_set_command_velocity(rmppacket_t *res, int trans, int rot) {
    res->pkt.Clear();
    res->pkt.id = 0x0413; // RMP_CAN_ID_COMMAND;
    res->pkt.PutSlot(0, (uint16_t)trans);
    res->pkt.PutSlot(1, (uint16_t)rot);
    res->pkt.PutSlot(2, (uint16_t)0); // RMP_CAN_CMD_NONE
    res->pkt.PutSlot(3, (uint16_t)0);
}


