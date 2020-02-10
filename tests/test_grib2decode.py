import pathlib

import pytest
import numpy as np

from gripy import py3grib2


def get_test_gfs_grib():
    data_dir = pathlib.Path(__file__).parent / 'data'
    grib_file = data_dir / 'gfs.t12z.pgrb2.0p25.f000.grb'
    assert grib_file.exists()
    return grib_file

def get_test_ngm_grib():
    data_dir = pathlib.Path(__file__).parent / 'data'
    grib_file = data_dir / 'navgem_2020020600f000.grib2'
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
    grib_file = get_test_ngm_grib()
    msgs = py3grib2.Grib2Decode(str(grib_file))
    for msg in msgs:
        assert msg.latitude_first_gridpoint > -90.1
        assert msg.latitude_last_gridpoint < 90.1


if __name__ == '__main__':
    test_grib2decode_gfs_values()
