import pathlib
import timeit

import pygrib
import cfgrib

import gripy.grib2

TESTDATADIR = pathlib.Path(__file__).parent.parent / 'tests/data'


def gripy_test():
    with gripy.grib2.Grib2File(TESTDATADIR / 'gfs.t12z.pgrb2b.0p25.f041') as g2:
        _ = [msg.shortname for msg in g2.grib_msgs]
        units = [msg.units for msg in g2.grib_msgs]
        assert units[150] == '%'
        assert len(g2.grib_msgs) == 396


def pygrib_test():
    g2 = pygrib.open(str(TESTDATADIR / 'gfs.t12z.pgrb2b.0p25.f041'))
    msgs = [msg for msg in g2]
    _ = [msg.shortName for msg in msgs]
    units = [msg.units for msg in msgs]
    assert units[150] == '%'
    assert len(msgs) == 396


def cfgrib_test():
    g2 = cfgrib.FileStream(str(TESTDATADIR / 'gfs.t12z.pgrb2b.0p25.f041'))
    _ = [msg.get('shortName') for msg in g2]
    units = [msg.get('units') for msg in g2]
    assert units[150] == '%'
    assert len(units) == 396


def benchmark_output(method, time_s):
    return (f"{method:6} = {time_s/n:05.4f}s ")


if __name__ == "__main__":
    n = 10
    pygrib_time = timeit.timeit("pygrib_test()",
                                number=n,
                                setup="from __main__ import pygrib_test")
    print(benchmark_output("pygrib", pygrib_time/n))
    gripy_time = timeit.timeit("gripy_test()",
                               number=n,
                               setup="from __main__ import gripy_test")
    print(benchmark_output("gripy", gripy_time/n))
    cfgrib_time = timeit.timeit("cfgrib_test()",
                                number=n,
                                setup="from __main__ import cfgrib_test")
    print(benchmark_output("cfgrib", cfgrib_time/n))
