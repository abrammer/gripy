__all__ = ["grib2", "libg2", "binary", "tables"]
from . import grib2


def open(filename):
    return grib2.Grib2File(filename)