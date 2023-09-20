# DT5495
Simple firmware with LEMO boards installed with fan-out in boards C-F

# Fan-out description
NIM channel 0 (the one on the far right) is given as output of the channels 00-07  
of the first IDC34 breakout cable (indication of channel numbers on the cable itself). 
In addition the same signal goes to the channel 16-17 (first entries of second cable). 
The remaining NIM channels from 1-6 are instead mapped 1 to 1 to the channels 18-23 of the 
IDC34 cable. In order to use them you simply have to connect everything to the IDC34 to SMA 
breakout board (8 channels per cable). 

# NIM/TTL
By default the handling of the signals is NIM based, however to handle TTL signals you can simply 
connect a LEMO cable between G0 and G1 LEMO sockets. The LEDS (OFF by default) will turn ON when the cable is
connected indicating the change in signal standards. 
