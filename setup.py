import setuptools
try:
    from numpy.distutils.core import setup, Extension
except ImportError:
    from setuptools import dist
    dist.Distribution().fetch_build_eggs(['numpy>=1.10'])
    from numpy.distutils.core import setup, Extension


lib = Extension(name='gripy.libg2', sources=['gripy/libg2.f90'])


setup (name = 'gripy',
       version = '0.0',
       install_requires=['numpy',],
       setup_requires=['numpy',],
       ext_modules = [lib,],
       description = '''Grib Reader In PYthon.,
           'Developmental repo for a python based grib reader. Replacing external deps with python''',
       packages = ['gripy',],
      )
