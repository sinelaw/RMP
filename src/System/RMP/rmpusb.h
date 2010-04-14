#ifndef _RMP_USB_H_
#define _RMP_USB_H_

extern "C" {
    
typedef struct rmpusb rmpusb_t;

typedef struct rmppacket rmppacket_t;


rmpusb_t *rmpusb_new();
void rmpusb_delete(rmpusb_t *rmpusb);
int rmpusb_write_packet(rmpusb_t *rmpusb, rmppacket_t *cnpkt);
int rmpusb_read_packet(rmpusb_t *rmpusb, rmppacket_t *cnpkt);

/******************************************************************************/

rmppacket_t *rmppacket_new();
void rmppacket_delete(rmppacket_t *pkt);
void rmppacket_set_command_velocity(rmppacket_t *res, int trans, int rot);

};

#endif
