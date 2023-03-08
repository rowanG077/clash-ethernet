import serial
from struct import pack, unpack
from time import sleep
import sys

READ = 0b0
WRITE = 0b1

w_req = []
for i in range(5**2):
     w_req.append(
          (READ, 0b1, i, 0xf)
     )
r_req = []
for i in range(5**2):
     r_req.append(
          (READ, 0b1, i, 0xf)
     )

# Check format is correct

def print_instruction(instruction):
    (instr, addr1, addr2, data) = instruction
    print(pack('>?', instr))
    print(pack('>B', addr1))
    print(pack('>B', addr2))
    print(pack('>H', data))
    print("_____________________________")

try:
    with serial.Serial('/dev/ttyUSB1', 9600, timeout=1) as ser:
        i = 0
    
        while ser.in_waiting == 0:
            ser.write(b'\x00')
            sleep(0.1)
            if i > 4:
                print("Couldn't find response from uart")
                sys.exit(1)
            i += 1
            
        #ser.read(1)
        
        for (instr, addr1, addr2, data) in w_req:
            ser.write(pack(">B", instr))
            ser.write(pack(">B", addr1))
            ser.write(pack(">B", addr2))
            ser.write(pack(">H", data))
            
        for (instr, addr1, addr2, data) in r_req:
            ser.write(pack(">B", instr))
            ser.write(pack(">B", addr1))
            ser.write(pack(">B", addr2))
            ser.write(pack(">H", data))
            (result, ) = unpack(">B", ser.read(1))
            (result2, ) = unpack(">B", ser.read(1))
            print("first byte: {}, second byte: {}".format(bin(result), bin(result2)))
            print("result: {}".format((result << 8) ^ result2))

except Exception as e:
    print(e)
    print("Could not open device at port (check that its plugged in)")
        
        
        
