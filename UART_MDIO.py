import serial
from struct import pack, unpack, iter_unpack
from time import sleep
import sys

READ = 0b0
WRITE = 0b1

requests = [
    (WRITE, 0b00001, 0b00010, 0xf),
    (READ, 0b00010, 0b00011, 0xf)
]

# Check format is correct
for (instr, addr1, addr2, data) in requests:
        print(pack('>?', instr))
        print(pack('>B', addr1))
        print(pack('>B', addr2))
        print(pack('>B', data >> 8))
        print(pack('>B', data & 0b1111))
        print("_____________________________")

try:
    with serial.Serial('/dev/ttyUSB0', 9600, timeout=1) as ser:
        
        i = 0
    
        while ser.in_waiting == 0:
            ser.write(b'\x00')
            sleep(0.1)
            if i > 4:
                print("Couldn't find response from uart")
                sys.exit(1)
            i += 1
            
        ser.read(1)
        
        for (instr, addr1, addr2, data) in requests:
            ser.write(pack(">B", instr))
            ser.write(pack(">B", addr1))
            ser.write(pack(">B", addr2))
            ser.write(pack(">B", data))
            
        (result, ) = unpack(">B", ser.read(1))

        print("result: {}".format(result))
except:
    print("Could not open device at port (check that its plugged in)")
        
        
        