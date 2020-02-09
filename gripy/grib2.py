import struct
from io import BytesIO

from gripy import g2pylib
from gripy import tables


class Grib2File:
    def __init__(self, filename):
        self.file_obj = open(filename,'rb')
        self.grib_msgs = []

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.file_obj.close()

    def read_metadata(self):
        nmsg = 0
        # loop over grib messages, read section 0, get entire grib message.
        while True:
            # find next occurence of string 'GRIB' (or EOF).
            nbyte = self.file_obj.tell()
            while True:
                self.file_obj.seek(nbyte)
                start = self.file_obj.read(4).decode('ascii','ignore')
                if start == '' or start == 'GRIB': break
                nbyte = nbyte + 1
            if start == '': break # at EOF
            # otherwise, start (='GRIB') contains indicator message (section 0)
            startpos = self.file_obj.tell()-4
            self.grib_msgs.append(Grib2Message(self.file_obj, startpos))
            print(self.grib_msgs[-1].section1.center)
        # if no grib messages found, nmsg is still 0 and it's not GRIB.
        if not self.grib_msgs:
            raise IOError('not a GRIB file')
        return nmsg


class Section1:
    def __init__(self, data, *_):
        if data[1] != 1:
            raise ValueError(data)
        self.center = tables.centers[data[2]]
        self.subcenter = data[3]
        self.tablesVersion = data[4]
        self.significanceOfReferenceTime = data[5]
        self.year = data[6]
        self.month = data[7]
        self.day = data[8]
        self.hour = data[9]
        self.minute = data[10]
        self.second = data[11]
        self.productionStatusOfProcessedData = data[12]
        self.typeOfProcessedData = data[13]


class Section2:
    def __init__(self, *_):
        pass


class Section3:
    def __init__(self, igds, grid_template, *_):
        self.grid_source = igds[0]
        self.ndpts = igds[1]
        self.num_oct = igds[2]
        self.intepret = igds[3]
        self.template_num = igds[4]
        self.grid_template = grid_template


class Section4:
    def __init__(self, template, template_num, *_):
        self.template_num = template_num
        self.pds_template = template


class Section5:
    def __init__(self, template, template_num, ndpts, pos):
        self.template_num = template_num
        self.drs_template = template


class Section6:
    def __init__(self, bmap, bmapflag):
        self.bmap = bmap
        self.bmapflag = bmapflag


class Section7:
    def __init__(self, io, pos):
        self.file_obj = io
        self.start_pos = pos


def _unpack2(*_):
    return (_,)

class Grib2Message:
    def __init__(self, io, startpos):
        self.file_obj = io
        self.start_byte = startpos
        self.lensect0 = 16
        self.lengrib = None
        self.section_lengths = {}
        self.read_section0()
        self.section1 = self.read_section_n(1)
        self.section2 = self.read_section_n(2)
        self.section3 = self.read_section_n(3)
        self.section4 = self.read_section_n(4)
        self.section5 = self.read_section_n(5)
        self.section6 = self.read_section6()
        self.section7 = self.read_section7()
        self.read_section8()

    def section_starting_position(self, section_num):
        pos = self.start_byte
        for n in range(section_num):
            pos += self.section_lengths[n]
            print(f"{n}, {self.section_lengths[n]}")
        return pos

    def _get_section_bytes(self, startpos):
        print(f'startpos {startpos}')
        self.file_obj.seek(startpos)
        lensect, sectnum = struct.unpack('>IB', self.file_obj.read(5))
        print(sectnum, lensect)
        self.file_obj.seek(startpos)
        return self.file_obj.read(lensect), sectnum

    def read_section0(self, ):
        startpos = self.section_starting_position(0) + 4
        self.file_obj.seek(startpos)
        # get discipline and version info.
        _, disc, vers, lengrib = struct.unpack('>HBBq', self.file_obj.read(12))
        if vers != 2:
            raise IOError('not a GRIB2 file (version number %d)' % vers)
        # we read 16 octets above, and need 4 at the end of the message
        self.file_obj.seek(lengrib-20, 1)
        # make sure the message ends with '7777'
        end = self.file_obj.read(4).decode('ascii','ignore')
        if end != '7777':
            raise IOError('partial GRIB message (no "7777" at end)')
        self.section_lengths[0] = 16
        self.lengrib = lengrib

    def read_section_n(self, n):
        section = {1: Section1,
                    2: Section2,
                    3: Section3,
                    4: Section4,
                    5: Section5}
        unpack = {1: g2pylib.unpack1,
                2: _unpack2,
                3: g2pylib.unpack3,
                4: g2pylib.unpack4,
                5: g2pylib.unpack5}

        startpos = self.section_starting_position(n)
        section_bytes, sectnum = self._get_section_bytes(startpos)
        if sectnum != n:
            self.section_lengths[n] = 0
            return section[n]()
        self.section_lengths[n] = len(section_bytes)
        return section[n](*unpack[n](section_bytes,0, []))

    def read_section6(self, ):
        n=6
        startpos = self.section_starting_position(n)
        section_bytes, sectnum = self._get_section_bytes(startpos)
        self.section_lengths[n] = len(section_bytes)
        bmap, bmapflag = g2pylib.unpack6(section_bytes,self.section3.ndpts, 0, [])
        return Section6(bmap, bmapflag )

    def read_section7(self, ):
        n=7
        startpos = self.section_starting_position(n)
        self.file_obj.seek(startpos)
        lensect, sectnum = struct.unpack('>IB', self.file_obj.read(5))
        print(sectnum, lensect)
        self.section_lengths[n] = lensect
        return Section7(self.file_obj, startpos)

    def read_section8(self, ):
        n=8
        startpos = self.section_starting_position(n)
        self.file_obj.seek(startpos)
        flag = self.file_obj.read(4).decode('ascii','ignore')
        if flag != '7777':
            raise IOError('Grib2 Message not finished yet')
