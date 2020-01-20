import setuptools
from numpy.distutils.core import setup, Extension

lib = Extension(name='gripy.libg2', sources=['gripy/libg2.f90'])


setup (name = 'gripy',
       version = '0.0',
       install_requires=['numpy', 'pupygrib',],
       ext_modules = [lib,],
       description = '''Grib Reader In PYthon.,
           'Developmental repo for a python based grib reader. Replacing external deps with python''',
       packages = ['gripy',],
      )
