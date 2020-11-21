import logging
import json
import pathlib
import struct

import numpy as np

from gripy import g2pylib
from gripy import tables, grids


class Grib2File:
    def __init__(self, filename):
        self.file_obj = open(filename, 'rb')
        self.grib_msgs = []
        self.fetch_messages()

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.file_obj.close()

    def fetch_messages(self):
        # loop over grib messages, read section 0, get entire grib message.
        while True:
            # find next occurence of string 'GRIB' (or EOF).
            nbyte = self.file_obj.tell()
            while True:
                self.file_obj.seek(nbyte)
                start = self.file_obj.read(4).decode('ascii', 'ignore')
                if start == '' or start == 'GRIB':
                    break
                nbyte = nbyte + 1
            if start == '':
                break  # at EOF
            # otherwise, start (='GRIB') contains indicator message (section 0)
            startpos = self.file_obj.tell() - 4
            self.grib_msgs.append(Grib2Message(self.file_obj, startpos))
        # if no grib messages found, nmsg is still 0 and it's not GRIB.
        if not self.grib_msgs:
            raise IOError('not a GRIB file')


class Section1:
    """ IDENTIFICATION SECTION:
    This section includes center, date, production status and
    which grib tables are used to decode the data """
    def __init__(self, data, *_):
        if data[1] != 1:
            raise ValueError(data)
        self.__data = data
        self.decoded_data = {}
        self.__center = data[2]
        self.subcenter = data[3]
        self.gMasterVersion = data[4]
        self.gLocalVersion = data[5]
        self.__significanceOfReferenceTime = data[6]
        self.year = data[7]
        self.month = data[8]
        self.day = data[9]
        self.hour = data[10]
        self.minute = data[11]
        self.second = data[12]
        self.__productionStatusOfProcessedData = data[13]
        self.__typeOfProcessedData = data[14]

    def __repr__(self, ):
        return (
            "Section 1:\n"
            f"  Originating Center : {self.center} - {self.subcenter} \n"
            f"  Grib Master Version: {self.gMasterVersion}.{self.gLocalVersion}\n"
            f"  Date               : {self.year}-{self.month:02}-{self.day:02}\n"
            f"  Time               : {self.hour:02}:{self.minute:02}:{self.second:02}\n"
            f"  Prod. Status       : {self.__productionStatusOfProcessedData}\n"
            f"  Data Type          : {self.__typeOfProcessedData}\n")

    @property
    def center(self, ):
        if 'center' not in self.decoded_data:
            self.decoded_data['center'] = tables.centers[self.__center]
        return self.decoded_data['center']

    @property
    def significanceOfReferenceTime(self, ):
        if 'significanceOfReferenceTime' not in self.decoded_data:
            self.decoded_data['significanceOfReferenceTime'] = get_table_value(
                '1.2', self.__significanceOfReferenceTime)[0]
        return self.decoded_data['significanceOfReferenceTime']

    @property
    def productionStatusOfProcessedData(self, ):
        if 'productionStatusOfProcessedData' not in self.decoded_data:
            self.decoded_data[
                'productionStatusOfProcessedData'] = get_table_value(
                    '1.3', self.__productionStatusOfProcessedData)[0]
        return self.decoded_data['productionStatusOfProcessedData']

    @property
    def typeOfProcessedData(self, ):
        if 'typeOfProcessedData' not in self.decoded_data:
            self.decoded_data['typeOfProcessedData'] = get_table_value(
                '1.4', self.__typeOfProcessedData)[0]
        return self.decoded_data['typeOfProcessedData']


class Section2:
    """ Local Use Section:
        Who knows what goes on in here, we'll just keep queit and move on """
    def __init__(self, *_):
        pass


class Section3:
    """ GRID DEFINITION SECTION:
    Includes metadata about the grid for the data. This is mainly just descriptors rather than
    the actual grid points.  This info will tell how to generate the grid locations"""
    def __init__(self, buff):
        self.data = buff
        self.header = struct.unpack_from('>IBBIBBH', buff, 0)
        if self.header[1] != 3:
            raise ValueError(
                f"Found Section {self.header[1]} while initialising section 3")
        self.grid_source = self.header[2]
        self.ndpts = self.header[3]
        self.num_oct = self.header[4]
        self.intepret = self.header[5]
        self.template_num = self.header[6]
        self._template_data = None

    @property
    def template(self):
        gdtmpl = g2pylib.grid_template(self.template_num)
        if gdtmpl is None:
            raise ValueError(
                "Grib Table 3.{tnum} not found".format(tnum=self.template_num))
            gdtmpl = {
                'name': 'Unknown Grid',
                'mapdrs': [],
                'mapgridlen': 1,
                'needext': 0
            }
        return gdtmpl

    @property
    def template_data(self, ):
        if self._template_data is None:
            gdtmpl = self.template
            fmt = gdtmpl['mapdrs']
            off = 14
            self._template_data = []
            for nbyte in fmt:
                self._template_data.append(
                    g2pylib.get_pp_bits(self.data, off, nbyte))
                off += abs(nbyte)
        return self._template_data

    @property
    def grid_type(self):
        return self.template['name']

    def latlon(self,):
        if self.template_num == 0:
            return grids.generate_regular_latlon_grid(self.ndpts, self.template_data)

    def __repr__(self):
        return ("Section 3:\n"
                f"  grid_source     : {self.grid_source}\n"
                f"  ndpts           : {self.ndpts}\n"
                f"  num_oct         : {self.num_oct}\n"
                f"  template_num    : {self.template_num}\n"
                f"  grid_template   : {self.template_data}\n")


class Section4:
    """PRODUCT DEFINITION SECTION:
    This includes details about the variable name, units, vertical coords.
    """
    def __init__(self, template, template_num, *_):
        self.template_num = template_num
        self.pds_template = template

    def __repr__(self):
        return ("Section 4:\n"
                f"  template number  = {self.template_num}\n"
                f"  template data    ={self.pds_template}\n")


class Section5:
    """DATA REPRESENTATION SECTION:
    Inludes data about how many data points to expect, type of data, and how to
    unpack the data"""
    def __init__(self, template, template_num, ndpts, pos):
        self.template_num = template_num
        self.drs_template = template
        self.ndpts = ndpts

    def __repr__(self, ):
        return ("Section 5:\n"
                f" template_num     : {self.template_num}\n"
                f" drs template     : {self.drs_template}\n"
                f" defined data     : {self.ndpts}\n")


class Section6:
    """BIT MAP SECTION:
    If the field has missing values, this section contains a binary mask of where those
    missing values are.  This is used to drop missing values in section 7"""
    def __init__(self, bmap, bmapflag):
        if bmapflag == 0:
            self.bmap = bmap.astype('B')
        else:
            self.bmap = []
        self.bmapflag = bmapflag

    def __repr__(self, ):
        return ("Section 6:\n"
                f"  bitmap length  : {len(self.bmap)}\n"
                f"  bitmap mask    : {len(self.bmap) - sum(self.bmap)}\n"
                f"  bitmap flag    :{self.bmapflag}\n")


class Section7:
    """DATA SECTION:
    This is where the data will be stored and unpacked from.
    """
    def __init__(self, io, pos, length):
        self.file_obj = io
        self.start_pos = pos
        self.section_length = length

    @property
    def byte_array(self,):
        self.file_obj.seek(self.start_pos)
        return self.file_obj.read(self.section_length)


def _unpack2(*_):
    return (_, )


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
        self.section3 = self.read_section3()
        self.section4 = self.read_section_n(4)
        self.section5 = self.read_section_n(5)
        self.section6 = self.read_section6()
        self.section7 = self.read_section7()
        self.decode_metadata()
        self.read_section8()

    def __repr__(self):
        return (
            f"\tlongname                = {self.longname}\n"
            f"\tunits                   = {self.units}\n"
            f"\tshortname               = {self.shortname}\n"
            f"\tGRIB2 Reference Time    = {self.section1.significanceOfReferenceTime}\n"
            f"\tGRIB2 Discipline        = {self.discipline_name()}\n"
            f"\tGRIB2 Category          = {self.category}\n"
            f"\tGRIB2 Production Status = {self.section1.productionStatusOfProcessedData}\n"
            f"\tGRIB2 Data Type         = {self.section1.typeOfProcessedData}\n"
        )

    def decode_metadata(self):
        pds_template = self.section4.pds_template
        self.category = get_table_value('4.1', pds_template[0])
        cat = pds_template[0]
        disc = self.discipline_int
        variable_data = get_table_value(f'4.2.{disc}.{cat}', pds_template[1])
        self.shortname = variable_data[2]
        self.units = variable_data[1]
        self.longname = variable_data[0]

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

    def discipline_name(self, ):
        # Code Table 0.0: Discipline of processed data in the GRIB message,
        # number of GRIB Master Table
        table0_0 = {
            0: 'Meteorological products',
            1: 'Hydrological products',
            2: 'Land surface products',
            3: 'Space products',
            4: 'Space weather products',
            10: 'Oceanographic products',
            209: 'Multi-Radar/Multi-Sensor (MRMS) products',
            255: 'Missing'
        }
        return table0_0.get(self.discipline_int, 'Missing')

    def read_section0(self, ):
        """ Section 0 'INDICATOR SECTION' describes the grib message
            This just includes the grib type (1/2), discipline and length of message"""
        startpos = self.section_starting_position(0) + 4
        self.file_obj.seek(startpos)
        # get discipline and version info.
        _, disc, vers, lengrib = struct.unpack('>HBBq', self.file_obj.read(12))
        if vers != 2:
            raise IOError('not a GRIB2 file (version number %d)' % vers)
        # we read 16 octets above, and need 4 at the end of the message
        self.file_obj.seek(lengrib - 20, 1)
        # make sure the message ends with '7777'
        end = self.file_obj.read(4).decode('ascii', 'ignore')
        if end != '7777':
            raise IOError('partial GRIB message (no "7777" at end)')
        self.section_lengths[0] = 16
        self.lengrib = lengrib
        self.discipline_int = disc

    def read_section_n(self, n):
        section = {
            1: Section1,
            2: Section2,
            3: Section3,
            4: Section4,
            5: Section5
        }
        unpack = {
            1: g2pylib.unpack1,
            2: _unpack2,
            3: g2pylib.unpack3,
            4: g2pylib.unpack4,
            5: g2pylib.unpack5
        }

        startpos = self.section_starting_position(n)
        section_bytes, sectnum = self._get_section_bytes(startpos)
        if sectnum != n:
            self.section_lengths[n] = 0
            return section[n]()
        self.section_lengths[n] = len(section_bytes)
        return section[n](*unpack[n](section_bytes, 0, []))

    def read_section3(self, ):
        n = 3
        startpos = self.section_starting_position(n)
        section_bytes, sectnum = self._get_section_bytes(startpos)
        self.section_lengths[n] = len(section_bytes)
        return Section3(section_bytes)

    def read_section6(self, ):
        n = 6
        startpos = self.section_starting_position(n)
        section_bytes, sectnum = self._get_section_bytes(startpos)
        self.section_lengths[n] = len(section_bytes)
        bmap, bmapflag = g2pylib.unpack6(section_bytes, self.section3.ndpts, 0,
                                         [])
        return Section6(bmap, bmapflag)

    def read_section7(self, ):
        n = 7
        startpos = self.section_starting_position(n)
        self.file_obj.seek(startpos)
        lensect, sectnum = struct.unpack('>IB', self.file_obj.read(5))
        self.section_lengths[n] = lensect
        return Section7(self.file_obj, startpos, lensect)

    def read_section8(self, ):
        """ END SECTION:
        Should just include '7777' to indicate the end of the grib message
        """
        n = 8
        startpos = self.section_starting_position(n)
        self.file_obj.seek(startpos)
        flag = self.file_obj.read(4).decode('ascii', 'ignore')
        if flag != '7777':
            raise IOError('Grib2 Message not finished yet')

    @property
    def values(self, ):
        fld = g2pylib.unpack7(gribmessage=self.section7.byte_array,
                              gdtnum=self.section3.template_num,
                              gdtmpl=self.section3.template_data,
                              drtnum=self.section5.template_num,
                              drtmpl=self.section5.drs_template,
                              ndpts=self.section5.ndpts)
        if self.section6.bmapflag == 0:
            vals = np.empty(self.section3.ndpts, 'f')
            vals[self.section6.bmap == 1] = fld
            vals[self.section6.bmap == 0] = np.nan
        else:
            vals = fld
        return vals


def get_table_value(table, key):
    table_data = tables.table4.get(table, None)
    if table_data is None:
        maj = table[0]
        table_dir = pathlib.Path(__file__).parent
        table_path = table_dir / f'tables/ncep/{maj}/{table}.json'
        if table_path.exists():
            with open(table_dir /
                      f'tables/ncep/{maj}/{table}.json') as json_file:
                table_data = json.load(json_file)
        else:
            logging.warning(f"{table_path} was not found.")
            table_data = {}
    tables.table4[table] = table_data
    attr_data = table_data.get(str(key), None)
    if attr_data is None:
        logging.warning(f"{key} not found, attributes will not be decoded")
        attr_data = str(key)
    return attr_data
