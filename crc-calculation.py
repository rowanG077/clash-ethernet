"""
    This file contains functionality for computing CRC checksums in an easy to understand and readable manner.
    At the bottom of the file we have some tests and we also test some packets that we know to be correct 
    from https://packetor.com/.

    Based on Clause 3. Media Access Control (MAC) frame and packet specifications.
"""

# Bit representation of x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1
POLY_BITS = '100000100110000010001110110110111'

def is_bitstring(b : str) -> bool:
    for c in b:
        if c not in '01':
            return False
    return True

def flip_bit(b):
    assert b in '01'
    return '0' if b == '1' else '1'

def flip_bits(bitstr : str) -> str:
    assert is_bitstring(bitstr)
    return ''.join([flip_bit(b) for b in bitstr])

def xor_bits(b1 : str, b2 : str) -> str:
    assert is_bitstring(b1) and is_bitstring(b2) and len(b1) == len(b2)
    return ''.join(['0' if b1[i] == b2[i] else '1' for i in range(len(b1))])

def hex_to_bits(hex_string : str) -> str:
    hex_string = hex_string.upper()
    hex_to_bit = {
        '0' : '0000',
        '1' : '0001',
        '2' : '0010',
        '3' : '0011',
        '4' : '0100',
        '5' : '0101',
        '6' : '0110',
        '7' : '0111',
        '8' : '1000',
        '9' : '1001',
        'A' : '1010',
        'B' : '1011',
        'C' : '1100',
        'D' : '1101',
        'E' : '1110',
        'F' : '1111',
    }
    return ''.join([hex_to_bit.get(h, '') for h in hex_string])

def flip_endiannes(bitstring : str) -> str:
    assert len(bitstring) % 8 == 0
    r = ''
    for i in range(int(len(bitstring) / 8)):        
        r += ''.join(reversed(bitstring[8*i:8*i+8]))
    return r

def poly_divide(bitstring : str, poly_bits : str):
    """Result is bitstring interpreted as a polynomial divided by the polynomial represented by poly_bits.
    Example: 101000 divided by 1011 (x^3 + x + 1) is 100."""
    assert is_bitstring(bitstring) and is_bitstring(poly_bits)

    length = max(0, len(bitstring) - len(poly_bits) + 1)
    for i in range(length):
        if bitstring[i] == '1':
            bitstring = bitstring[:i] + xor_bits(bitstring[i:i + len(poly_bits)], poly_bits) + bitstring[i + len(poly_bits):]
    return bitstring[length:]

def calc_crc(bitstring : str) -> str:
    """Implementation of clause 3.2.9 Frame Check Sequence (FCS) field.
    Returns the CRC of a bitstring. Bytes insinde the bitstring should be LSB first (as stated in clause 3.3)."""
    assert is_bitstring(bitstring)

    # complement the first 32 bits (3.2.9a).
    bitstring = flip_bits(bitstring[:32]) + bitstring[32:]

    # multiply input by x^32 (3.2.9c).
    bitstring += '0' * 32

    # divide by polynomial x32 + x26 + x23 + x22 + x16 + x12 + x11 + x10 + x8 + x7 + x5 + x4 + x2 + x + 1 (3.2.9c).
    result = poly_divide(bitstring, POLY_BITS) 
    assert len(result) == 32

    # CRC is the complement of the result (3.2.9e).
    return flip_bits(result)

def update_state(state : str, new_byte : str) -> str:
    """ Helper function for calc_crc_sm. Related to 3.2.9c) """
    assert is_bitstring(state) and is_bitstring(new_byte) and len(state) == 32 and len(new_byte) == 8

    state += new_byte # In haskell we might need a 5 byte buffer.
    for i in range(8):
        if state[i] == '1':
            # xor the current and next 32 bits with the polynomial. 
            state = state[:i] + xor_bits(state[i:i+len(POLY_BITS)], POLY_BITS) + state[i+len(POLY_BITS):]
        
    # Throw away the first byte in the state (the oldest byte that we received)
    # Alternatively, we can immediatly throw away a bit if we received it (do this inside the loop for updating the state).
    return state[8:] 

def calc_crc_sm(bitstring : str) -> str:
    """  Implementation of clause 3.2.9 Frame Check Sequence (FCS) field.
        Implemented as a state machine that processes one byte at a time.
        The bytes are assumed to be LSB first (as stated in clause 3.3)."""    
    assert is_bitstring(bitstring) and len(bitstring) % 8 == 0

    state_val = ''
    state_counter = 0
    for i in range(int(len(bitstring) / 8)):
        new_byte = bitstring[8*i:8*i + 8]
        if state_counter < 4: # fill the buffer and complement it. (3.2.9a)
            state_val += flip_bits(new_byte)
            state_counter += 1
        else:
            state_val = update_state(state_val, new_byte) # (3.2.9c)

    # Clause 3.2.9c) says we should multiply by x^32. That is the same as receiving another 32 0 bits. Or 4 NULL bytes.
    for i in range(4):
        state_val = update_state(state_val, '0' * 8)
    return flip_bits(state_val) # (3.2.9e)

######################################################
# TESTS This is just a helper file for understanding
# CRC, so we do not include these in the unittests.
######################################################

def test_packet_crc(complete_packet : str):
    assert is_bitstring(complete_packet) and len(complete_packet) % 8 == 0 and len(complete_packet) >= 32
    
    check_value = complete_packet[len(complete_packet)-32:]
    packet = complete_packet[:len(complete_packet) -32]

    # Output test results and halt if they are incorrect.
    print(f"Expected: {check_value}")
    print(f"Buffered: {calc_crc(packet)}")
    print(f"Streamed: {calc_crc_sm(packet)}")
    assert check_value == calc_crc(packet) and check_value == calc_crc_sm(packet)
    
# Tests for hex_to_bits (https://www.binaryhexconverter.com/hex-to-binary-converter):
assert hex_to_bits("309605283800309 6E   6FC39080045C") == "00110000100101100000010100101000001110000000000000110000100101101110011011111100001110010000100000000000010001011100"
assert hex_to_bits("0004D0002 0000F  D06A 6C40A220001") == "00000000000001001101000000000000001000000000000000001111110100000110101001101100010000001010001000100000000000000001"
assert hex_to_bits("3A0102010    0172AFAAF3C7484B998E") == "00111010000000010000001000000001000000000001011100101010111110101010111100111100011101001000010010111001100110001110"
assert hex_to_bits("3a0102010    0172aFAaF3c7484b998e") == "00111010000000010000001000000001000000000001011100101010111110101010111100111100011101001000010010111001100110001110"
assert hex_to_bits("F4E501810177   BD000000D0A0D0A50 ") == "11110100111001010000000110000001000000010111011110111101000000000000000000000000110100001010000011010000101001010000"
assert hex_to_bits("617373776F7 264207265  717569726 ") == "01100001011100110111001101110111011011110111001001100100001000000111001001100101011100010111010101101001011100100110"
assert hex_to_bits("5642C20627574206E6F6E65207365 zzz") == "01010110010000101100001000000110001001110101011101000010000001101110011011110110111001100101001000000111001101100101"

# flip_endiannes
assert flip_endiannes("10110111") == "11101101"
assert flip_endiannes("11111111") == "11111111"
assert flip_endiannes("00000000") == "00000000"
assert flip_endiannes("10101011") == "11010101"

# test for poly_divide (https://en.wikipedia.org/wiki/Cyclic_redundancy_check):
assert poly_divide('11010011101100000', '1011') == '100'
assert poly_divide('11000000', '1011') == '100'

test_packet_crc(flip_endiannes(hex_to_bits("""
    01 00 5E 00 00 6B 00 80 63 00 09 BA 08 00 45 00 
    00 52 45 9F 00 00 01 11 D0 E2 C0 A8 02 06 E0 00
    00 6B 01 3F 01 3F 00 3E 00 00 12 02 00 36 00 00
    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 80
    63 FF FF 00 09 BA 00 01 9E 48 05 0F 00 00 45 B1
    11 51 04 72 F9 C1 00 00 00 00 00 00 00 00 00 00
    e2 10 d1 25""")))

print('')

test_packet_crc(flip_endiannes(hex_to_bits(""" 
    00 00 01 00 00 01 00 10 94 00 00 02 08 00 45 00
    00 3D 00 0E 00 00 0A 11 2F 4A C0 55 01 02 C0 00
    00 01 04 00 0E C8 00 29 72 31 20 44 05 21 00 00
    00 01 00 00 00 00 00 0F 42 40 00 0F 42 40 00 00
    00 00 01 09 02 73 65 63 72 65 74 FA 7B 79 1C 1c 
    df 44 21""")))

print('')

test_packet_crc(flip_endiannes(hex_to_bits(""" 
    00 E0 1C 3C 17 C2 00 1F 33 D9 81 60 08 00 45 00 
    00 80 00 00 40 00 40 11 24 55 0A 0A 01 01 0A 0A 
    01 04 00 35 DB 66 00 6C 2D 2D 79 56 81 80 00 01 
    00 02 00 02 00 00 04 6D 61 69 6C 08 70 61 74 72 
    69 6F 74 73 02 69 6E 00 00 01 00 01 C0 0C 00 05 
    00 01 00 00 2A 4B 00 02 C0 11 C0 11 00 01 00 01 
    00 00 2A 4C 00 04 4A 35 8C 99 C0 11 00 02 00 01 
    00 01 43 8C 00 06 03 6E 73 32 C0 11 C0 11 00 02 
    00 01 00 01 43 8C 00 06 03 6E 73 31 C0 11 df 73 
    c3 15
    """)))

# As stated in clause 3.3: each octet of the mac frame, with exception of FCS, is transmitted LSB first.
# Currenltly we flip the endiannes of all the bytes that we get from packetor and this matches.
# This is a bit confusing because we flip all bytes and do not exept the FCS.
# We need better understanding of why and when we need to flip in order to program this in a state machine matter for haskell.