import setuptools
from distutils.core import setup

from distutils.command.sdist import sdist
try:
    from numpy.distutils.core import setup, Extension
except ImportError:
    from setuptools import dist
    dist.Distribution().fetch_build_eggs(['numpy>=1.10'])
    from numpy.distutils.core import setup, Extension


lib = Extension(name='gripy.libg2', sources=['gripy/libg2.f90'])


setup(
    name='gripy',
    author='abrammer',
    author_email='alan.brammer@gmail.com',
    url='https://github.com/abrammer/gripy',
    version='0.0.4',
    install_requires=[
        'numpy',
    ],
    setup_requires=[
        'numpy',
    ],
    ext_modules=[
        lib,
    ],
    description='''Grib Reader In PYthon.,
            Developmental repo for a python based grib reader.
            Replacing external deps with python''',
    packages=['gripy', 'gripy.tables', 'gripy.legacy'],
    package_data={
        "gripy.tables": ["*/*/*.json"],
    },
    cmdclass={'sdist': sdist},
)
