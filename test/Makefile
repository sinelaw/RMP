all: test-libftdi

test-kvaser: test-kvaser.cpp 
	g++ test-kvaser.cpp -lftd2xx -lcanlib

test-libftdi: test-libftdi.c
	gcc test-libftdi.c -lftdi -o test-libftdi