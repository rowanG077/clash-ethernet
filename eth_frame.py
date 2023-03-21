import socket

"""
This is for sending from Python to the FPGA.

To bind an IP to your device use: 
    sudo ip addr add 100.1.1.1 dev eno1

    
Problems: the Python scripts executes without error.
But we see the packet on the lo device, instead of eno1.
"""

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)  # Internet(4), UDP.
s.connect(('100.1.1.1', 3239)) # IP, Port

s.send(("Hello World").encode('utf-8'))

s.close()