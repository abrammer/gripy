import warnings

import numpy as np


def generate_regular_latlon_grid(ndpts, grid_template):
    """ Generate simple 1d lat, lon arrays for regular grid definition
    """
    # earth_shape = grid_template[0]  # 6 = sphere of 6,371,229.0 m
    nlon = grid_template[7]
    nlat = grid_template[8]
    angle_scale = grid_template[9]
    angle_divis = grid_template[10]
    if angle_scale == 0:
        angle_scale = 1.
    if angle_divis <= 0 or angle_divis == (2 ** 32 - 1):
        angle_divis = 1.e6
    offset = angle_scale / angle_divis
    lat1 = grid_template[11] * offset
    lat2 = grid_template[14] * offset
    lon1 = grid_template[12] * offset
    lon2 = grid_template[15] * offset
    dlon = grid_template[16] * offset
    dlat = grid_template[17] * offset
    lats, dlat_compute = np.linspace(lat1,
                                     lat2,
                                     num=nlat,
                                     endpoint=True,
                                     retstep=True)
    if dlat_compute != dlat:
        warnings.warn("Computed lat delta doesn't match template")
    lons, dlon_compute = np.linspace(lon1,
                                     lon2,
                                     num=nlon,
                                     endpoint=True,
                                     retstep=True)
    if dlon_compute != dlon:
        warnings.warn("Computed lon delta doesn't match template")
    return lats, lons
