import setuptools
from distutils.core import setup

from distutils.command.sdist import sdist
from numpy.distutils.core import setup, Extension


lib = Extension(name='gripy.libg2', sources=['gripy/libg2.f90'], extra_f90_compile_args=["-fallow-argument-mismatch"])


setup(
    ext_modules=[lib, ],
    cmdclass={'sdist': sdist},
)
