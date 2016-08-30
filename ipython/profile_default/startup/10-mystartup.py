#   ~/.ipython/profile_default/startup/10-mystartup.py
# from https://gist.github.com/taldcroft/547c0b6e0ae15e0c970d

from IPython.core.magic import register_line_magic

ip = get_ipython()

# try:
#     import line_profiler
#     ip.define_magic('lprun', line_profiler.magic_lprun)
# except ImportError:
#     pass


def _import_numpy():
    ip.ex('import numpy as np')
    ip.ex('from numpy import ma')
    ip.ex('print("Numpy", np.__version__)')


def _import_astropy():
    _import_numpy()
    ip.ex('import astropy')
    ip.ex('print("Astropy", astropy.__version__)')
    ip.ex('import astropy.units as u')
    ip.ex('from astropy.io import ascii, fits')
    ip.ex('from astropy.wcs import WCS')
    ip.ex('from astropy.table import Table, Column, MaskedColumn')
    # ip.ex('from astropy.time import Time, TimeDelta')
    # ip.ex('from astropy.coordinates import SkyCoord, ICRS, FK4, FK5')


def _import_mpdaf():
    _import_astropy()
    ip.ex('import mpdaf')
    ip.ex('print("MPDAF", mpdaf.__version__)')
    ip.ex('from mpdaf.obj import Cube, Image, Spectrum, WCS, WaveCoord')
    ip.ex('from mpdaf.drs import PixTable')
    ip.ex('from mpdaf.sdetect import Source, Catalog')


@register_line_magic
def numpy(line):
    """Import Numpy"""
    _import_numpy()


@register_line_magic
def astropy(line):
    """Import Astropy"""
    _import_astropy()


@register_line_magic
def mpdaf(line):
    """Import MPDAF"""
    _import_mpdaf()


@register_line_magic
def imu(line):
    ip.ex('import ipython_memory_usage.ipython_memory_usage as imu')
    ip.ex('imu.start_watching_memory()')


del numpy, astropy, mpdaf, imu
