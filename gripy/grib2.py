import json
import pathlib
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
        print(data)
        self.center = tables.centers[data[2]]
        self.subcenter = data[3]
        self.gMasterVersion = data[4]
        self.gLocalVersion = data[5]
        self.significanceOfReferenceTime = data[6]
        self.year = data[7]
        self.month = data[8]
        self.day = data[9]
        self.hour = data[10]
        self.minute = data[11]
        self.second = data[12]
        self.productionStatusOfProcessedData = data[13]
        self.typeOfProcessedData = data[14]

    def __repr__(self,):
        return (f"Originating Center  : {self.center} - {self.subcenter} \n"
                f"Grib Master Version: {self.gMasterVersion}.{self.gLocalVersion}\n"
                f"Date               : {self.year}-{self.month:02}-{self.day:02}\n"
                f"Time               : {self.hour:02}:{self.minute:02}:{self.second:02}\n"
                )


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
        self.decode_metadata()
        self.read_section8()

    def __repr__(self):
        return (f"\tlongname                = {self.longname}\n"
                f"\tunits                   = {self.units}\n"
                f"\tshortname               = {self.shortname}\n"
                f"\tGRIB2 Reference Time    = {self.significanceOfReferenceTime}\n"
                f"\tGRIB2 Discipline        = {self.discipline_name()}\n"
                f"\tGRIB2 Category          = {self.category}\n"
                f"\tGRIB2 Production Status = {self.productionStatusOfProcessedData}\n"
                f"\tGRIB2 Data Type         = {self.typeOfProcessedData}\n"
                )

    def decode_metadata(self):
        table_dir = pathlib.Path(__file__).parent
        with open(table_dir / f'tables/ncep/1/1.2.json') as json_file:
            table = json.load(json_file)
        self.significanceOfReferenceTime = table[str(self.section1.significanceOfReferenceTime)][0]
        with open(table_dir / f'tables/ncep/1/1.3.json') as json_file:
            table = json.load(json_file)
        self.productionStatusOfProcessedData = table[str(self.section1.productionStatusOfProcessedData)][0]
        with open(table_dir / f'tables/ncep/1/1.4.json') as json_file:
            table = json.load(json_file)
        self.typeOfProcessedData = table[str(self.section1.typeOfProcessedData)][0]

        pds_template = self.section4.pds_template
        with open(table_dir / f'tables/ncep/4/4.1.json') as json_file:
            table = json.load(json_file)
        self.category = table[str(pds_template[0])]
        cat = pds_template[0]
        with open(table_dir / f'tables/ncep/4/4.2.0.{cat}.json') as json_file:
            table = json.load(json_file)
        variable_data = table[str(pds_template[1])]
        self.shortname = variable_data[2]
        self.units = variable_data[1]
        self.longname = variable_data[0]
        print(variable_data)

    def section_starting_position(self, section_num):
        pos = self.start_byte
        for n in range(section_num):
            pos += self.section_lengths[n]
        return pos

    def _get_section_bytes(self, startpos):
        self.file_obj.seek(startpos)
        lensect, sectnum = struct.unpack('>IB', self.file_obj.read(5))
        self.file_obj.seek(startpos)
        return self.file_obj.read(lensect), sectnum

    def discipline_name(self,):
        #Code Table 0.0: Discipline of processed data in the GRIB message, number of GRIB Master Table
        table0_0 = {0:'Meteorological products',
                    1:'Hydrological products',
                    2:'Land surface products',
                    3:'Space products',
                    4:'Space weather products',
                    10:'Oceanographic products',
                    209:'Multi-Radar/Multi-Sensor (MRMS) products',
                    255:'Missing'}
        return table0_0.get(self.discipline_int, 'Missing')

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
        self.discipline_int = disc

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
