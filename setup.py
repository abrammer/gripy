import subprocess
from distutils.core import setup

from distutils.command.sdist import sdist
from numpy.distutils.core import setup, Extension


f90flags = []
if int(subprocess.check_output(["gcc", "-dumpversion"])) >= 10:
    f90flags += ["-fallow-argument-mismatch"]

lib = Extension(
    name="gripy.libg2",
    sources=["gripy/libg2.f90"],
    extra_f90_compile_args=f90flags,
)


setup(
    ext_modules=[
        lib,
    ],
    cmdclass={"sdist": sdist},
)
