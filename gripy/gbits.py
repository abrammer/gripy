import numpy as np


def ishft(val, bitcnt):
    print("ishft")
    print(f"bitcnt: {bitcnt}")
    print(np.unpackbits(np.uint8(val)))

    if bitcnt > 0:
        retval = val << bitcnt
    elif bitcnt < 0:
        retval = val >> bitcnt
    else:
        retval = val
    print(np.unpackbits(np.uint8(retval)))
    return retval

def iand(val1, val2):
    print("iand")
    print(np.unpackbits(np.uint8(val1)))
    print(np.unpackbits(np.uint8(val2)))
    return val1 & val2


def ior(val1, val2):
    print("ior")
    print(np.unpackbits(np.uint8(val1)))
    print(np.unpackbits(np.uint8(val2)))
    return val1 | val2

def ones(n):
    print(f"{n}: {(2<<n) - 1}")
    return (2<<n) - 1


def gbits(buff, pos, nbits: int, nskip:int, n_num:int):
    iout = []
    nbit = pos
    nbyte = nbits
    for i in range(0, n_num):
        print(f"_______ {i}")
        bitcnt = nbits
        index = int(nbit // 8)
        ibit = int(nbit % 8)
        nbit = nbit + nbyte + nskip
        print(f"nbit: {nbit}")
#       first byte
        tbit = np.min([bitcnt, 8-ibit])
        itmp = iand((buff[index]), ones(8-ibit))
        
        if (tbit != 8-ibit):
            itmp = ishft(itmp, tbit-8+ibit)

        index = index + 1
        bitcnt = bitcnt - tbit
        print(f"index: {index}")
        print(f"bitcnt: {bitcnt}")
#       now transfer whole bytes
        while (bitcnt == 8):
            itmp = ior(ishft(itmp, 8), (buff[index]))
            bitcnt = bitcnt - 8
            index = index + 1

        print(f"index: {index}")
        print(f"bitcnt: {bitcnt}")
#       get data from last byte
        if (bitcnt > 0):
            itmp = ior(ishft(itmp, bitcnt), 
                       iand(ishft(buff[index], -(8-bitcnt)), ones(bitcnt))
                      )
    
        iout.append(itmp)
    return np.array(iout, dtype=np.int32)
