#   ~/.ipython/profile_default/startup/10-mystartup.py
# from https://gist.github.com/taldcroft/547c0b6e0ae15e0c970d

ip = get_ipython()

try:
    import line_profiler
    ip.define_magic('lprun', line_profiler.magic_lprun)
except ImportError:
    pass


def _import_astropy(self, arg):
    ip.ex('import numpy as np')
    ip.ex('import astropy')
    ip.ex('print("Astropy", astropy.__version__)')
    ip.ex('from astropy.io import ascii, fits')
    ip.ex('from astropy.wcs import WCS')
    # ip.ex('from astropy import table')
    ip.ex('from astropy.table import Table, QTable, Column, MaskedColumn')
    # ip.ex('from astropy.table.table_helpers import TimingTables, simple_table, complex_table')
    # ip.ex('from astropy.time import Time, TimeDelta')
    # ip.ex('from astropy.coordinates import SkyCoord, ICRS, FK4, FK5')
    ip.ex('import astropy.units as u')


def _import_mpdaf(self, arg):
    _import_astropy(self, arg)
    ip.ex('import mpdaf')
    ip.ex('print("MPDAF", mpdaf.__version__)')
    ip.ex('from mpdaf.obj import Cube, Image, Spectrum, WCS')
    ip.ex('from mpdaf.drs import PixTable')


ip.define_magic('astro', _import_astropy)
ip.define_magic('mpdaf', _import_mpdaf)
