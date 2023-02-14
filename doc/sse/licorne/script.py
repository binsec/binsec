DATA1 = open('bloc1.bin', 'rb').read()
DATA2 = open('bloc2.bin', 'rb').read()
assert not ( len(DATA1) % 4)
assert not ( len(DATA2) % 4)
DATA1 = [DATA1[k:k + 4] for k in range(0, len(DATA1),4)]
DATA2 = [DATA2[k:k + 4] for k in range(0, len(DATA2),4)]

def arm_code(n):
    j = 0xd
    i = 0x25
    with open('arm' + str(n) + ".bin", "wb") as f:
        while (i != 0x6a5):
            codeBytes = int.from_bytes(DATA1[(i & 0x7f) + n * 0x100], 'little') ^ int.from_bytes(DATA2[n * 0x100 + (j & 0x7f)], 'little')
            codeBytes = int.to_bytes(codeBytes,4, 'little')
            i += 0xd
            j += 0x25
            f.write(codeBytes)

for k in range(8):
    arm_code(k)