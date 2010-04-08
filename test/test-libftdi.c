/* simple.c

   Simple libftdi usage example

   This program is distributed under the GPL, version 2
*/

#include <stdio.h>
#include <unistd.h>
#include <ftdi.h>

char calc_checksum(char *data, int data_len) {
    unsigned short checksum;
    unsigned short checksum_hi;
    int i;
    if (data_len != 18) {
        return 0; /* illegal length */
    }
    checksum = 0;
    for(i = 0; i < 17; i++)
    {
        checksum += data[i];
    }
    checksum_hi = (unsigned short)(checksum >> 8);
    checksum &= 0xff;
    checksum += checksum_hi;
    checksum_hi = (unsigned short)(checksum >> 8);
    checksum &= 0xff;
    checksum += checksum_hi;
    checksum = (~checksum + 1) & 0xff;
    return (char)checksum;
}

int main(void)
{
    int ret;
    struct ftdi_context ftdic;
    char data[18]; // 18 is message size from can-usb rmp thingy
    int i, k, num_read, num_write, checksum;
    double msecs = 0;
    
    if (ftdi_init(&ftdic) < 0)
    {
        fprintf(stderr, "ftdi_init failed\n");
        return EXIT_FAILURE;
    }

    if ((ret = ftdi_usb_open(&ftdic, 0x0403, 0xe729)) < 0)
    {
        fprintf(stderr, "unable to open ftdi device: %d (%s)\n", ret, ftdi_get_error_string(&ftdic));
        return EXIT_FAILURE;
    }

    if ((ret = ftdi_usb_reset(&ftdic)) < 0) {
        fprintf(stderr, "Failed to reset device\n");
        return EXIT_FAILURE;
    }
    // Read out FTDIChip-ID of R type chips
    printf("Chip type: %d ", ftdic.type);
    switch (ftdic.type) {
    case TYPE_AM: printf("TYPE_AM"); break;
    case TYPE_BM: printf("TYPE_BM"); break;
    case TYPE_2232C: printf("TYPE_2232C"); break;
    case TYPE_R: printf("TYPE_R"); break;
    case TYPE_2232H: printf("TYPE_2232H"); break;
    case TYPE_4232H: printf("TYPE_4232H"); break;
    }
    printf("\n");
    if (ftdic.type == TYPE_R)
    {
        unsigned int chipid;
        printf("ftdi_read_chipid: %d\n", ftdi_read_chipid(&ftdic, &chipid));
        printf("FTDI chipid: %X\n", chipid);
    }
/*
    if (0 != ftdi_write_data_set_chunksize(&ftdic, 4096)) {
        printf("Error setting write chunk size\n");
        goto EXIT;
    }
    if (0 != ftdi_read_data_set_chunksize(&ftdic, 4096)) {
        printf("Error setting read chunk size\n");
        goto EXIT;
    }
*/
    while (1) {
        num_read = ftdi_read_data(&ftdic, data, sizeof data);
        if (num_read < 0) {
            printf("Error reading: %d\n", num_read);
            break;
        }
        if (num_read == 0) {
            continue;
        }
        printf("\n%d:\t", num_read);
        for (i = 0; i < sizeof data; i++) {
            for (k = 0; k < 18; k++) {
                printf("%02hhX ", data[i]);
                i++;
            }
            printf("\n\t");
        }

        /* 0x0413 = RMP CONTROL MESSAGE */
        {
            char write_data[18] = {0xf0, 0x55, 0x00, 0x00, 0x00, 0x00, 0x04, 0x13, 0x00, /* Velocity */ 0x00, 0x00, 0x00, /* turn */ 0xc0, 0x00, };
            write_data[17] = calc_checksum(write_data, 18);
            num_write = ftdi_write_data(&ftdic, write_data, 18);
            printf("Wrote: %d, checksum: %02hhX", num_write, write_data[17]);

    if (msecs < 30) {
        usleep((30-msecs)*1000);
//        usleep(100*1000);
    }
}
        
    }
EXIT:
    if ((ret = ftdi_usb_close(&ftdic)) < 0)
    {
        fprintf(stderr, "unable to close ftdi device: %d (%s)\n", ret, ftdi_get_error_string(&ftdic));
        return EXIT_FAILURE;
    }

    ftdi_deinit(&ftdic);

    return EXIT_SUCCESS;
}
