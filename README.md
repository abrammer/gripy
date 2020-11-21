### GRIPY  -- (Grid Reader In PYthon)

The original aim was to keep it 100% python, but pulling in fortran makes it 500x faster, so maybe I'll change the name.

This is an experimental repo for building a more easily installable grib reader in python.

This should not be used as is, only install to help on development.  Things will change, things are broken.

This will likely never replace an eccodes based grib reader.  GRIB2 is a crazy format that can house many compression, resolution, data types in a single file.  However many operational files are simple, this repo is for quick pip install access to those easy to decode files.


#### Current Capabilities:
 - Index and decode product metadata from messages.
 - Decode and unpack data for simple-packed fields.


#### Missing Capabilities:
 - Decoding grid information in a useful manner.
 - Decoding most compression types.


#### Up Next:
 - Decoding grid info for rectilinear grids (GFS/ GEFS files)
 - Incorporating S3 file-object access
