import subprocess
from distutils.core import setup

from distutils.command.sdist import sdist
from numpy.distutils.core import setup, Extension


def get_gcc_majorversion():
    version_string = subprocess.check_output(["gcc", "-dumpversion"]).decode()
    major = version_string.split('.', maxsplit=1)[0]
    return int(major)


f90flags = []
if get_gcc_majorversion() >= 10:
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
