import serial
from struct import pack, unpack
from time import sleep
import sys

ADD = b'\x00'
SUB = b'\x01'
XOR = b'\x02'

def instr_to_str(instr):
    if instr == ADD:
        return "ADD"
    elif instr == SUB:
        return "SUB"
    elif instr == XOR:
        return "XOR"

program = [
    (ADD, 10, 30),
    (SUB, 120, 10),
    (XOR, 0xf0, 0x0f),
]

with serial.Serial('/dev/ttyUSB0', 9600, timeout=1) as ser:
    # Hacky homing sequence.
    # Write 0x00 until we are in known CPUIDLe state
    i = 0
    while ser.in_waiting == 0:
        ser.write(b'\x00')
        sleep(0.1)
        if i > 4:
            print("Couldn't find response from uartCPU")
            sys.exit(1)

        i += 1
    
    ser.read(1)

    for (instr, a, b) in program:
        ser.write(instr)
        ser.write(pack(">B", a))
        ser.write(pack(">B", b))

        (result, ) = unpack(">B", ser.read(1))

        print(f"{instr_to_str(instr)} {a:3} {b:3} = {result:3}")
