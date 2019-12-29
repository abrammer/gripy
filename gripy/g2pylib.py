import struct

import numpy as np
import pupygrib

from gripy.comunpack import comunpack


def mkieee(num):
    bits, = struct.unpack('>I', struct.pack('>f', num))
    return bits


def rdieee(num):
    sign = num >> 31
    num &= 0x7FFFFFFF
    bits, = struct.unpack('>f', struct.pack('>I', num))
    if sign:
        return -bits
    else:
        return bits


def itor_ieee(ra):
    return_flts = np.array(ra.shape, dtype=np.float32)
    for i, num in enumerate(ra):
        return_flts[i] = rdieee(num)
    return return_flts


def rtoi_ieee(ra):
    return_ints = np.array(ra.shape, dtype=np.int32)
    for i, num in enumerate(ra):
        newint = mkieee(num)
        try:
            return_ints[i] = newint
        except OverflowError:
            return_ints[i] = np.int64(newint).astype(np.int32)
    return return_ints


def grid_template(template_number):
    templatesgrid = {
        # 3.0: Lat/Lon grid
        0 :{ 'mapgridlen':19, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1] },
        # 3.1: Rotated Lat/Lon grid
        1 :{ 'mapgridlen':22, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4] },
        # 3.2: Stretched Lat/Lon grid
        2 :{ 'mapgridlen':22, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,-4] },
        # 3.3: Stretched & Rotated Lat/Lon grid
        3 :{ 'mapgridlen':25, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4,-4,4,-4] },
        # Added GDT 3.4,3.5    (08/05/2013)
        # 3.4: Variable resolution Latitude/Longitude
        4 :{ 'mapgridlen':13, 'needext':1, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,1,1] },
        # 3.5: Variable resolution rotate Latitude/Longitude
        5 :{ 'mapgridlen':16, 'needext':1, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,1,1,-4,4,4] },
        # 3.12: Transverse Mercator
        12 :{ 'mapgridlen':22, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,1,-4,4,4,1,4,4,-4,-4,-4,-4] },
        # 3.101: General unstructured grid
        101 :{ 'mapgridlen':4, 'needext':0, 'mapdrs':[1,4,1,-4] },
        # 3.140: Lambert Azimuthal Equal Area Projection
        140 :{ 'mapgridlen':17, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,4,4,1,4,4,1] },
        # 3.10: Mercator
        10 :{ 'mapgridlen':19, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,1,-4,-4,4,1,4,4,4] },
        # 3.20: Polar Stereographic Projection
        20 :{ 'mapgridlen':18, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,1,-4,4,4,4,1,1] },
        # 3.30: Lambert Conformal
        30 :{ 'mapgridlen':22, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,1,-4,4,4,4,1,1,-4,-4,-4,4] },
        # 3.31: Albers equal area
        31 :{ 'mapgridlen':22, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,1,-4,4,4,4,1,1,-4,-4,-4,4] },
        # 3.40: Guassian Lat/Lon
        40 :{ 'mapgridlen':19, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1] },
        # 3.41: Rotated Gaussian Lat/Lon
        41 :{ 'mapgridlen':22, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4] },
        # 3.42: Stretched Gaussian Lat/Lon
        42 :{ 'mapgridlen':22, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,-4] },
        # 3.43: Stretched and Rotated Gaussian Lat/Lon
        43 :{ 'mapgridlen':25, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,-4,4,4,-4,4,-4] },
        # 3.50: Spherical Harmonic Coefficients
        50 :{ 'mapgridlen':5, 'needext':0, 'mapdrs':[4,4,4,1,1] },
        # 3.51: Rotated Spherical Harmonic Coefficients
        51 :{ 'mapgridlen':8, 'needext':0, 'mapdrs':[4,4,4,1,1,-4,4,4] },
        # 3.52: Stretched Spherical Harmonic Coefficients
        52 :{ 'mapgridlen':8, 'needext':0, 'mapdrs':[4,4,4,1,1,-4,4,-4] },
        # 3.53: Stretched and Rotated Spherical Harmonic Coefficients
        53 :{ 'mapgridlen':11, 'needext':0, 'mapdrs':[4,4,4,1,1,-4,4,4,-4,4,-4] },
        # 3.90: Space View Perspective or orthographic
        90 :{ 'mapgridlen':21, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,1,4,4,4,4,1,4,4,4,4] },
        # 3.100: Triangular grid based on an icosahedron
        100 :{ 'mapgridlen':11, 'needext':0, 'mapdrs':[1,1,2,1,-4,4,4,1,1,1,4] },
        # 3.110: Equatorial Azimuthal equidistant
        110 :{ 'mapgridlen':16, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,-4,4,1,4,4,1,1] },
        # 3.120: Azimuth-range projection
        120 :{ 'mapgridlen':7, 'needext':1, 'mapdrs':[4,4,-4,4,4,4,1] },
        # 3.204: Curvilinear Orthogonal Grid
        204 :{ 'mapgridlen':19, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1] },
        # 3.32768: Rot Lat/Lon E-grid (Arakawa)
        32768 :{ 'mapgridlen':19, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1] },
        # 3.32769: Rot Lat/Lon Non-E Staggered grid (Arakawa)
        32769 :{ 'mapgridlen':21, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,4,-4,4,1,-4,4,4,4,1,4,4] },
        # 3.1000: Cross Section Grid
        1000 :{ 'mapgridlen':20, 'needext':1, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,-4,4,1,4,4,1,2,1,1,2] },
        # 3.1100: Hovmoller Diagram Grid
        1100 :{ 'mapgridlen':28, 'needext':0, 'mapdrs':[1,1,4,1,4,1,4,4,4,4,-4,4,1,-4,4,1,4,1,-4,1,1,-4,2,1,1,1,1,1] },
        # 3.1200: Time Section Grid
        1200 :{ 'mapgridlen':16, 'needext':1, 'mapdrs':[4,1,-4,1,1,-4,2,1,1,1,1,1,2,1,1,2] },
    }
    return templatesgrid[template_number]


def drs_template(template_number):
    drs_templates = {
        # // 5.0: Grid point data - Simple Packing
         0: {'mapdrslen': 5, 'needext':0, 'mapdrs':[4,-2,-2,1,1] },
            #  // 5.2: Grid point data - Complex Packing
         2: { 'mapdrslen': 16, 'needext':0, 'mapdrs':[4,-2,-2,1,1,1,1,4,4,4,1,1,4,1,4,1] },
            #  // 5.3: Grid point data - Complex Packing and spatial differencing
         3: { 'mapdrslen': 18, 'needext':0, 'mapdrs':[4,-2,-2,1,1,1,1,4,4,4,1,1,4,1,4,1,1,1], 'fmt':'>IhhBBBBIIIBBIBIBBB'},
            #  // 5.50: Spectral Data - Simple Packing
         50: { 'mapdrslen': 5, 'needext':0, 'mapdrs':[4,-2,-2,1,4] },
            #  // 5.51: Spherical Harmonics data - Complex packing
         51:{  'mapdrslen': 10, 'needext':0, 'mapdrs':[4,-2,-2,1,-4,2,2,2,4,1] },
            #  // 5.1: Matrix values at gridpoint - Simple packing
        #  1:{  'mapdrslen': 15, 'needext':1, 'mapdrs':[4,-2,-2,1,1,1,4,2,2,1,1,1,1,1,1] },
            #  // 5.40: Grid point data - JPEG2000 encoding
         40:{  'mapdrslen': 7, 'needext':0, 'mapdrs':[4,-2,-2,1,1,1,1] },
            #  // 5.41: Grid point data - PNG encoding
         41:{  'mapdrslen': 5, 'needext':0, 'mapdrs':[4,-2,-2,1,1] },
            #  // 5.40000: Grid point data - JPEG2000 encoding
         40000:{ 'mapdrslen': 7,'needext': 0, 'mapdrs':[4,-2,-2,1,1,1,1] },
            #  // 5.40010: Grid point data - PNG encoding
         40010:{ 'mapdrslen': 5, 'needext':0, 'mapdrs':[4,-2,-2,1,1] },
        }
    return drs_templates[template_number]


def pds_template(template_number):
    templatespds = {
            # 4.0: Analysis or Forecast at Horizontal Level/Layer
            #      at a point in time
        0:{'len':15,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4] },
            # 4.1: Individual Ensemble Forecast at Horizontal Level/Layer
            #      at a point in time
        1:{'len':18,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1] },
            # 4.2: Derived Fcst based on whole Ensemble at Horiz Level/Layer
            #      at a point in time
        2:{'len':17,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1] },
            # 4.3: Derived Fcst based on Ensemble cluster over rectangular
            #      area at Horiz Level/Layer at a point in time
        3:{'len':31,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,-4,4,4,1,-1,4,-1,4] },
            # 4.4: Derived Fcst based on Ensemble cluster over circular
            #      area at Horiz Level/Layer at a point in time
        4:{'len':30,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,4,4,1,-1,4,-1,4] },
            # 4.5: Probablility Forecast at Horiz Level/Layer
            #      at a point in time
        5:{'len':22,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,-1,-4,-1,-4] },
            # 4.6: Percentile Forecast at Horiz Level/Layer
            #      at a point in time
        6:{'len':16,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1] },
            # 4.7: Analysis or Forecast Error at Horizontal Level/Layer
            #      at a point in time
        7:{'len':15,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4] },
            # 4.8: Ave/Accum/etc... at Horiz Level/Layer
            #      in a time interval
        8:{'len':29,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.9: Probablility Forecast at Horiz Level/Layer
            #      in a time interval
        9:{'len':36,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,-1,-4,-1,-4,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.10: Percentile Forecast at Horiz Level/Layer
            #       in a time interval
        10:{'len':30,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.11: Individual Ensemble Forecast at Horizontal Level/Layer
            #       in a time interval
        11:{'len':32,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.12: Derived Fcst based on whole Ensemble at Horiz Level/Layer
            #       in a time interval
        12:{'len':31,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.13: Derived Fcst based on Ensemble cluster over rectangular
            #       area at Horiz Level/Layer in a time interval
        13:{'len':45,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,-4,4,4,1,-1,4,-1,4,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.14: Derived Fcst based on Ensemble cluster over circular
            #       area at Horiz Level/Layer in a time interval
        14:{'len':44,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,1,1,1,1,-4,4,4,1,-1,4,-1,4,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.15: Average, accumulation, extreme values or other statistically-processed values over a
            # spatial area at a horizontal level or in a horizontal layer at a point in time
        15:{'len':18,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1] },
            # 4.20: Radar Product
        20:{'len':19,'needext':0, 'mapdrs':[1,1,1,1,1,-4,4,2,4,2,1,1,1,1,1,2,1,3,2] },
            # 4.30: Satellite Product
        30:{'len':5,'needext':1, 'mapdrs':[1,1,1,1,1] },
            # 4.31: Satellite Product
        31:{'len':5,'needext':1, 'mapdrs':[1,1,1,1,1] },
            # 4.40: Analysis or forecast at a horizontal level or in a horizontal layer
            # at a point in time for atmospheric chemical constituents
        40:{'len':16,'needext':0, 'mapdrs':[1,1,2,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4] },
            # 4.41: Individual ensemble forecast, control and perturbed, at a horizontal level or
            # in a horizontal layer at a point in time for atmospheric chemical constituents
        41:{'len':19,'needext':0, 'mapdrs':[1,1,2,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1] },
            # 4.42: Average, accumulation, and/or extreme values or other statistically-processed values
            # at a horizontal level or in a horizontal layer in a continuous or non-continuous
            # time interval for atmospheric chemical constituents
        42:{'len':30,'needext':1, 'mapdrs':[1,1,2,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.43: Individual ensemble forecast, control and perturbed, at a horizontal level
            # or in a horizontal layer in a continuous or non-continuous
            # time interval for atmospheric chemical constituents
        43:{'len':33,'needext':1, 'mapdrs':[1,1,2,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.254: CCITT IA5 Character String
        254:{'len':3,'needext':0, 'mapdrs':[1,1,4] },
            # 4.1000: Cross section of analysis or forecast
            #         at a point in time
        1000:{'len':9,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4] },
            # 4.1001: Cross section of Ave/Accum/etc... analysis or forecast
            #         in a time interval
        1001:{'len':16,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,4,1,1,1,4,1,4] },
            # 4.1001: Cross section of Ave/Accum/etc... analysis or forecast
            #         over latitude or longitude
        1002:{'len':15,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,1,1,4,4,2] },
            # 4.1100: Hovmoller-type grid w/ no averaging or other
            #         statistical processing
        1100:{'len':15,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4] },
            # 4.1100: Hovmoller-type grid with averaging or other
            #         statistical processing
        1101:{'len':22,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,4,1,1,1,4,1,4] },
            # 4.32:Simulate (synthetic) Satellite Product
        32:{'len':10,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,2,1] },
            # 4.44: Analysis or forecast at a horizontal level or in a horizontal layer
            # at a point in time for Aerosol
        44:{'len':21,'needext':0, 'mapdrs':[1,1,2,1,-1,-4,-1,-4,1,1,1,2,1,1,2,1,-1,-4,1,-1,-4] },
            # 4.45: Individual ensemble forecast, control and
            # perturbed,  at a horizontal level or in a horizontal layer
            # at a point in time for Aerosol
        45:{'len':24,'needext':0, 'mapdrs':[1,1,2,1,-1,-4,-1,-4,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1] },
            # 4.46: Ave or Accum or Extreme value at level/layer
            # at horizontal level or in a horizontal in a continuous or
            # non-continuous time interval for Aerosol
        46:{'len':35,'needext':1, 'mapdrs':[1,1,2,1,-1,-4,-1,-4,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # 4.47: Individual ensemble forecast, control and
            # perturbed, at horizontal level or in a horizontal
            # in a continuous or non-continuous time interval for Aerosol
        47:{'len':38,'needext':1, 'mapdrs':[1,1,1,2,1,-1,-4,-1,-4,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            #             PDT 4.48
            # 4.48: Analysis or forecast at a horizontal level or in a horizontal layer
            # at a point in time for Optical Properties of Aerosol
        48:{'len':26,'needext':0, 'mapdrs':[1,1,2,1,-1,-4,-1,-4,1,-1,-4,-1,-4,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4] },
            #             VALIDATION --- PDT 4.50
            # 4.50: Analysis or forecast of multi component parameter or
            # matrix element at a point in time
        50:{'len':21,'needext':0, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,4,4,4,4] },
            #             VALIDATION --- PDT 4.52
            # 4.52: Analysis or forecast of Wave parameters
            # at the Sea surface at a point in time
        52:{'len':15,'needext':0, 'mapdrs':[1,1,1,1,1,1,1,1,2,1,1,4,1,-1,-4] },

            # 4.51: Categorical forecasts at a horizontal level or
            # in a horizontal layer at a point in time
        51:{'len':16,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1] },

            # 4.91: Categorical forecasts at a horizontal level or
            # in a horizontal layer at a point in time
            # in a continuous or non-continuous time interval
        91:{'len':36,'needext':1, 'mapdrs':[1,1,2,1,-1,-4,-1,-4,1,-1,-4,-1,-4,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4] },
            # PDT 4.33  (07/29/2013)
            # 4.33: Individual ensemble forecast, control, perturbed,
            # at a horizontal level or in a  horizontal layer
            # at a point in time for simulated (synthetic) Satellite data
        33:{'len':18,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,2,2,2,-1,-4,1,1,1] },
            # PDT 4.34  (07/29/2013)
            # 4.34: Individual ensemble forecast, control, perturbed,
            # at a horizontal level or in a  horizontal layer,in a continuous or
            # non-continuous interval for simulated (synthetic) Satellite data
        34:{'len':32,'needext':1, 'mapdrs':[1,1,1,1,1,2,1,1,4,1,2,2,2,-1,-4,1,1,1,2,1,1,1,1,1,1,4,1,1,1,4,1,4] },
            # PDT 4.53  (07/29/2013)
            # 4.53:  Partitioned parameters at
            # horizontal level or horizontal layer
            # at a point in time
        53:{'len':19,'needext':1, 'mapdrs':[1,1,1,1,4,2,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4] },
            # PDT 4.54  (07/29/2013)
            # 4.54: Individual ensemble forecast, control, perturbed,
            # at a horizontal level or in a  horizontal layer
            # at a point in time for partitioned parameters
        54:{'len':22,'needext':1, 'mapdrs':[1,1,1,1,4,2,1,1,1,2,1,1,4,1,-1,-4,1,-1,-4,1,1,1] },
        }
    return templatespds[template_number]


def get_pp_bits(buf, offset, nbits):
    formats = {
        -1:pupygrib.binary.unpack_int8_from,
        -2:pupygrib.binary.unpack_int16_from,
        -3:pupygrib.binary.unpack_int24_from,
        -4:pupygrib.binary.unpack_int32_from,
        1:pupygrib.binary.unpack_uint8_from,
        2:pupygrib.binary.unpack_uint16_from,
        3:pupygrib.binary.unpack_uint24_from,
        4:pupygrib.binary.unpack_uint32_from,
        5:pupygrib.binary.unpack_uint64_from,
    }
    bits = formats[nbits](buf, offset)
    return bits


def get_format(nbits):
    formats = {
        -1:"b",
        -2:"h",
        -3:"hb",
        -4:"i",
        -5:"q"  ,
        1:"B",
        2:"H",
        3:"HB",
        4:"I",
        5:"Q",
    }
    format_str = ''.join([formats[nbit] for nbit in nbits])
    return f">{format_str}"


def unpack1(gribmsg,pos):
    """unpack section 1 given starting point in bytes
    used to test pyrex interface to g2_unpack1"""
    header = struct.unpack_from('>IBhhBBBhBBBBBBB', gribmsg, pos)
    # length, secnum, center_id, subcenter, grib_table, gribaug_table, timesig,\
    #     year, mm,dd,hh,mn, ss, prod_status, data_type, extra = header
    # idsect = []
    # pos = pos + 5
    # idsect.append(struct.unpack('>h',gribmsg[pos:pos+2])[0])
    # pos = pos + 2
    # idsect.append(struct.unpack('>h',gribmsg[pos:pos+2])[0])
    # pos = pos + 2
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>h',gribmsg[pos:pos+2])[0])
    # pos = pos + 2
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    # idsect.append(struct.unpack('>B',gribmsg[pos:pos+1])[0])
    # pos = pos + 1
    return np.array(header[2:],dtype=np.int32), pos+header[0]


def unpack3(buff, pos, y):
    header = struct.unpack_from('>IBBIBBH', buff, pos)
    length = header[0]
    secnum = header[1]
    if secnum != 3:
        raise ValueError('Got Section {secnum} not Section 3'.format(secnum=secnum))
    grid_source = header[2]
    ndpts = header[3]
    num_oct = header[4]
    interpret = header[5]
    template_num = header[6]
    igds = np.array([grid_source, ndpts, num_oct, interpret, template_num], dtype=np.int32)

    gdtmpl = grid_template(template_num)
    fmt = gdtmpl['mapdrs']
    off = pos+14
    template = []
    for nbyte in fmt:
        template.append(get_pp_bits(buff, off, nbyte))
        off += abs(nbyte)

    deflist = np.array([], dtype=np.int32)
    if num_oct != 0 or gdtmpl['needext']:
        # TODO: Implement Section 3 Template Extensions
        # TODO: Implement Section 3 Optional List Numbers
        print("Not supported")

    return igds, np.array(template, dtype=np.int32), deflist,  pos+length,


def unpack4(buff, pos, y):
    header = struct.unpack_from('>IBHH', buff, pos)
    length = header[0]
    secnum = header[1]
    if secnum != 4:
        raise ValueError('Got Section {secnum} not Section 4'.format(secnum=secnum))
    nvals = header[2]
    template_num = header[3]
    pdstmpl = pds_template(template_num)
    fmt = pdstmpl['mapdrs']
    off = pos + 9
    template = []
    for nbyte in fmt:
        template.append(get_pp_bits(buff, off, nbyte))
        off += abs(nbyte)
    if nvals != 0 or pdstmpl['needext']:
        # TODO: Implement Section 4 Template Extensions
        # TODO: Implement Section 4 Optional Coords
        print("Not supported")
    return np.array(template, dtype=np.int32), template_num, np.array([], dtype=np.int32), pos+length


def unpack5(buff, pos, y):
    _header = struct.unpack_from('>IBIH', buff, pos)
    length = _header[0]
    secnum = _header[1]
    if secnum != 5:
        raise ValueError('Got Section {secnum} not Section 5'.format(secnum=secnum))
    ndpts =  _header[2]
    temp_no = _header[3]
    drstmpl = drs_template(temp_no)
    fmt = drstmpl['mapdrs']
    off = pos + 11
    template = []
    for nbyte in fmt:
        template.append(get_pp_bits(buff, off, nbyte))
        off += abs(nbyte)
    #template = np.array(struct.unpack_from(drs_template(temp_no)['fmt'], buff[11:]), dtype=np.int32)

    return np.array(template, dtype=np.int32), temp_no, ndpts, pos+length


def unpack6(buff, gds1, pos, y):
    _header = struct.unpack_from('>IBB', buff, pos)
    # length = _header[0]
    secnum = _header[1]
    if secnum != 6:
        raise ValueError('Got Section {secnum} not Section 6'.format(secnum=secnum))
    bitmap_flag =  _header[2]
    if bitmap_flag == 255:
        bitmap = None
    else:
        # membuff = np.array(memoryview(buff[pos+6:pos+length]), dtype=np.uint8)
        # fmt = f">{gds1/8:.0f}B"
        # membuff = np.array(struct.unpack_from(fmt, buff, pos+6), dtype=np.uint8)
        membuff = np.frombuffer(buff, dtype='>B', count=int(gds1/8), offset=pos+6)
        bitmap = np.unpackbits(membuff)
        if len(bitmap) != gds1:
            raise RuntimeError('Section 6: Bitmap length does not match expected')

    return bitmap, bitmap_flag


def unpack7(gribmessage, gdtnum, gdtmpl,drtnum, drtmpl,ndpts,ipos, zeros,printminmax=False,storageorder='C'):
    fld = g2_unpack7(gribmessage, ipos, gdtnum, gdtmpl, drtnum, drtmpl, ndpts)
    return fld

def g2_unpack7(cgrib, iofst, igdsnum, igdstmpl, idrsnum, idrstmpl, ngpts):
    lensec, isecnum = struct.unpack_from('>IB', cgrib, iofst)
    # octet_cnt = int((lensec-5))
    # membuff = np.frombuffer(cgrib, dtype='>B', count=octet_cnt, offset=iofst+5)
    # bit_buff = np.unpackbits(membuff)
    if isecnum != 7:
        raise ValueError('Got Section {secnum} not Section 7'.format(secnum=isecnum))
    if idrsnum == 0:
        # fld = simunpack(cgrib[ipos:],idrstmpl,ndpts)
        raise NotImplementedError('Simunpack is not support yet, sorry :/ ')
    elif (idrsnum == 2) or (idrsnum == 3):
        # print("cgrib:", len(cgrib[iofst+5:]))
        # raise NotImplementedError('comunpack is not support yet, sorry :/ ')
        fld = comunpack(cgrib[iofst+5:], lensec, idrsnum, idrstmpl, ngpts)
#         fld = comunpack(bit_buff, lensec, idrsnum, idrstmpl, ngpts)
    elif (idrsnum == 50):           # Spectral Simple
        # simunpack(cgrib[ipos:],idrstmpl,ndpts-1,lfld+1)
        # rdieee(idrstmpl+4,lfld+0,1);
        raise NotImplementedError('Simunpack is not support yet, sorry :/ ')
    else:
        raise NotImplementedError('Everything is not support yet, sorry :/ ')
    return fld
