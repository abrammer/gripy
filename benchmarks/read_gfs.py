from datetime import datetime, timedelta
import pathlib
import shutil
import timeit

import requests
import pygrib
import cfgrib

import gripy.grib2

TESTDATADIR = pathlib.Path(__file__).parent.parent / 'tests/data'


def download_gfs_file():
    local_file = TESTDATADIR / "gfs.t00z.pgrb2b.0p25.f041"
    if local_file.exists():
        return
    date = datetime.utcnow() - timedelta(days=1)
    date = date.replace(hour=0, minute=0)
    url = f"https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.{date:%Y%m%d/%H/gfs.t%Hz}.pgrb2b.0p25.f041"
    with requests.get(url, stream=True) as r:
        r.raise_for_status()
        with open(local_file, 'wb') as f:
            shutil.copyfileobj(r.raw, f)


def gripy_test():
    with gripy.grib2.Grib2File(TESTDATADIR / 'gfs.t00z.pgrb2b.0p25.f041') as g2:
        _ = [msg.shortname for msg in g2.grib_msgs]
        units = [msg.units for msg in g2.grib_msgs]
        assert units[150] == '%'
        assert len(g2.grib_msgs) == 396


def pygrib_test():
    g2 = pygrib.open(str(TESTDATADIR / 'gfs.t00z.pgrb2b.0p25.f041'))
    msgs = [msg for msg in g2]
    _ = [msg.shortName for msg in msgs]
    units = [msg.units for msg in msgs]
    assert units[150] == '%'
    assert len(msgs) == 396


def cfgrib_test():
    g2 = cfgrib.FileStream(str(TESTDATADIR / 'gfs.t00z.pgrb2b.0p25.f041'))
    _ = [msg.get('shortName') for msg in g2]
    units = [msg.get('units') for msg in g2]
    assert units[150] == '%'
    assert len(units) == 396


def benchmark_output(method, time_s):
    return (f"{method:6} = {time_s:05.4f}s ")


def main():
    download_gfs_file()
    print("Downloaded File")
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


if __name__ == "__main__":
    main()
