import struct

import pytest
import numpy as np

from gripy import py3grib2
from gripy import g2pylib
from gripy.libg2 import comunpack
from gripy.libg2 import gbytes as gbits


def test_py3_ieeeint_round_trip():
    tests = np.array([-256.56, -10.3, 0.5, 10.6, 100.6, 234327832], dtype=np.float32)
    for test in tests:
        ieeeint = py3grib2._putieeeint(test)
        return_float = py3grib2._getieeeint(ieeeint)
        assert return_float == test


def test_put_ieeeint():
    expected_output = np.array([-1054867456, 0, 1092616192, 1120403456, 1298102514], dtype=np.int32)
    test_input = np.array([-10, 0, 10, 100., 234327832], dtype=np.float32)

    for _input, _expect in zip(test_input, expected_output):
        returned = py3grib2._putieeeint(_input)
        assert returned == _expect


def test_get_ieeeint():
    test_input = np.array([-1054867456, 0, 1092616192, 1120403456, 1298102514], dtype=np.int32)
    expected_output = np.array([-10, 0, 10, 100, 234327832], dtype=np.float32)

    for _input, _expect in zip(test_input, expected_output):
        returned = py3grib2._getieeeint(_input)
        assert returned == _expect


def test_section6_decode():
    def make_sect6_buff(ndpts):
        if ndpts==0:
            length = 6
            secnum = 6
            flag = 255
            buff = struct.pack('>IBB', length, secnum, flag)
        else:
            length = 6 + int(ndpts/8)
            secnum = 6
            flag = 0
            ones = np.ones((int(ndpts/16),), dtype=int) * 255
            zeros = np.zeros((int(ndpts/16),), dtype=int)
            bitmap = np.concatenate((ones, zeros))
            buff = struct.pack(f">I{((ndpts/8)+2):.0f}B", length, secnum, flag, *bitmap)
        return buff

    buff = make_sect6_buff(0)
    bitmap, bitmapflag = g2pylib.unpack6(buff, 0, 0, 0)
    assert bitmap == None
    assert bitmapflag == 255

    ndpts = 1038240
    npts2 = int(ndpts / 2)
    buff = make_sect6_buff(ndpts)
    bitmap, bitmapflag  = g2pylib.unpack6(buff, ndpts, 0, 0)
    assert bitmap.shape == (1038240,)
    assert all(bitmap[:npts2] == 1)
    assert all(bitmap[npts2:] == 0)
    assert bitmapflag == 0


def test_gbits_int():
        _input = np.array([0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,  1, 0, 1, 0, 0, 0, 0, 0,], dtype=np.uint8)
        __input = np.packbits(_input)
        retval = gbits(__input, 2, 9, 0, 2)
        assert retval == pytest.approx(np.array([160,160]))

        
def test_gbits_hex():
        _input =np.frombuffer(b'\x0e\xb7', np.uint8)
        retval = gbits(_input, 0, 16, 0, 1)
        assert retval == pytest.approx(np.array([3767,]))


if __name__ == "__main__":
    pytest.main()
#     import ncepgrib2
#     import pupygrib
#     import g2clib
#     import struct
#     import numpy as np
#     import py3grib2
#     import pygrib
#     import pathlib
#     import logging
#     logging.getLogger().setLevel(logging.INFO)
#     FORMAT = '%(asctime)-15s | %(filename)s +%(lineno)s | %(message)s'
#     logging.basicConfig(format=FORMAT)
    
#     _dir = pathlib.Path(__file__).parent.resolve()
# #     msgs3 = py3grib2.Grib2Decode(_dir.parent/'gfs.t12z.pgrb2.0p25.f000')
# #     msgs = ncepgrib2.Grib2Decode(_dir.parent/'gfs.t12z.pgrb2.0p25.f000')
# #     for msg, msg3 in zip(msgs3, msgs):
# #         msg_v =msg.values
# #         msg3_v = msg3.values
# #         print(np.max(abs(msg_v - msg3_v)))

# #     msgs = py3grib2.Grib2Decode(_dir.parent/'gfs.t12z.pgrb2.0p25.f000')
# #     for msg, msg3 in zip(msgs3[2:6], msgs[2:6]):
# #         msg_v =msg.values
# #         msg3_v = msg3.values
# #         print(np.max(abs(msg_v - msg3_v)))

#     msgs = py3grib2.Grib2Decode(_dir.parent/'gfs.t12z.pgrb2.0p25.f000')
#     for msg in msgs[:20]:
#         msg_v = msg.values
#         print(np.max(msg_v))

#     with pygrib.open(str(_dir.parent/'gfs.t12z.pgrb2.0p25.f000')) as pg:
#         for msg in pg[:20]:
#             msg_v = msg.values
#             print(np.max(msg_v))
