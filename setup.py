from setuptools import setup


setup (name = 'gripy',
       version = '0.0',
       install_requires=['numpy', 'pupygrib',],
       description = '''Grib Reader In PYthon.,
           'Developmental repo for a python based grib reader. Replacing external deps with python''',
       packages = ['gripy',],
      )
