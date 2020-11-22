import pathlib

import pytest
import numpy as np

from gripy.legacy import py3grib2
from gripy import grib2


def get_test_gfs_grib():
    data_dir = pathlib.Path(__file__).parent / 'data'
    grib_file = data_dir / 'gfs.t12z.pgrb2.0p25.f000.grb'
    assert grib_file.exists()
    return grib_file


def get_test_gfs42_grib():
    data_dir = pathlib.Path(__file__).parent / 'data'
    grib_file = data_dir / 'gfs.t12z.pgrb2.0p25.f042'
    assert grib_file.exists()
    return grib_file


def get_test_nav_grib():
    data_dir = pathlib.Path(__file__).parent / 'data'
    grib_file = data_dir / 'navgem_2020020800f000.grib2'
    assert grib_file.exists()
    return grib_file


def test_grib2decode_gfs_prod_def():
    grib_file = get_test_gfs_grib()
    msgs = py3grib2.Grib2Decode(str(grib_file))
    assert len(msgs) == 2

    expected_prod_def = [
        [2, 2, 2, 0, 81, 0, 0, 1, 0, 103, 0,  10, 255, 0, 0],
        [2, 3, 2, 0, 81, 0, 0, 1, 0, 103, 0,  10, 255, 0, 0],
    ]
    for expect, ret_msg in zip(expected_prod_def, msgs):
        assert all(expect == ret_msg.product_definition_template)


def test_grib2_gfs_prod_def():
    grib_file = get_test_gfs_grib()
    with grib2.Grib2File(str(grib_file)) as g2:
        msgs = g2.grib_msgs
    assert len(msgs) == 2

    expected_prod_def = [
        [2, 2, 2, 0, 81, 0, 0, 1, 0, 103, 0,  10, 255, 0, 0],
        [2, 3, 2, 0, 81, 0, 0, 1, 0, 103, 0,  10, 255, 0, 0],
    ]
    for expect, ret_msg in zip(expected_prod_def, msgs):
        assert all(expect == ret_msg.section4.pds_template)
        assert ret_msg.discipline_int == 0
        assert ret_msg.discipline_name() == 'Meteorological products'


def test_grib2_gfs_pds_decode():
    grib_file = get_test_gfs_grib()
    with grib2.Grib2File(str(grib_file)) as g2:
        msgs = g2.grib_msgs
    assert len(msgs) == 2

    expected_shortname = ['UGRD', 'VGRD']
    for expect, ret_msg in zip(expected_shortname, msgs):
        print(ret_msg)
        assert expect == ret_msg.shortname

    expected_longname = ['U-component of wind', 'V-component of wind']
    for expect, ret_msg in zip(expected_longname, msgs):
        assert expect == ret_msg.longname


def test_grib2_gfs42_pds_decode():
    grib_file = get_test_gfs42_grib()
    with grib2.Grib2File(str(grib_file)) as g2:
        msgs = g2.grib_msgs
    assert len(msgs) == 1

    expected_shortname = ['CLWMR']
    for expect, ret_msg in zip(expected_shortname, msgs):
        print(ret_msg)
        print(ret_msg.section1)
        print(ret_msg.section4)
        assert expect == ret_msg.shortname

    expected_longname = ['Cloud mixing ratio']
    for expect, ret_msg in zip(expected_longname, msgs):
        assert expect == ret_msg.longname


def test_grib2_nav_pds_decode():
    grib_file = get_test_nav_grib()
    with grib2.Grib2File(str(grib_file)) as g2:
        msgs = g2.grib_msgs
    assert len(msgs) == 1

    expected_shortname = ['TMP', ]
    for expect, ret_msg in zip(expected_shortname, msgs):
        print(ret_msg)
        assert expect == ret_msg.shortname

    expected_longname = ['Temperature', ]
    for expect, ret_msg in zip(expected_longname, msgs):
        assert expect == ret_msg.longname


def test_grib2_nav_latlon():
    grib_file = get_test_nav_grib()
    with grib2.Grib2File(str(grib_file)) as g2:
        msgs = g2.grib_msgs
        lats, lons = msgs[0].section3.latlon()
    assert lats[0] > -90.1
    assert lats[-1] < 90.1


def test_grib2decode_gfs_ident_sect():
    grib_file = get_test_gfs_grib()
    msgs = py3grib2.Grib2Decode(str(grib_file))

    expected_ident_sect = [
        [7, 0, 2, 1, 1, 2020, 1, 17, 12, 0, 0, 0, 1],
        [7, 0, 2, 1, 1, 2020, 1, 17, 12, 0, 0, 0, 1],
    ]
    for expect, ret_msg in zip(expected_ident_sect, msgs):
        assert all(expect == ret_msg.identification_section)


def test_grib2decode_gfs_values():
    grib_file = get_test_gfs_grib()
    msgs = py3grib2.Grib2Decode(str(grib_file))

    expected_vals = [{'min': -25.66,
                      'max': 29.63,
                      'mean': 0.06,
                      },
                     {'min': -29.44,
                      'max': 25.76,
                      'mean': -0.04,
                      }, ]
    for expect, ret_msg in zip(expected_vals, msgs):
        values = ret_msg.values
        assert expect['min'] == pytest.approx(np.min(values), abs=0.1)
        assert expect['max'] == pytest.approx(np.max(values), abs=0.1)
        assert expect['mean'] == pytest.approx(np.mean(values), abs=0.1)


def test_grib2decode_ngm_latlon():
    grib_file = get_test_nav_grib()
    msg = py3grib2.Grib2Decode(str(grib_file))
    assert msg.latitude_first_gridpoint > -90.1
    assert msg.latitude_last_gridpoint < 90.1




if __name__ == '__main__':
    pytest.main()
