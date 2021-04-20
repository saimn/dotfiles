#   ~/.ipython/profile_default/startup/10-mystartup.py
# from https://gist.github.com/taldcroft/547c0b6e0ae15e0c970d

from IPython.core.magic import register_line_magic

ip = get_ipython()  # noqa

# try:
#     import line_profiler
#     ip.define_magic('lprun', line_profiler.magic_lprun)
# except ImportError:
#     pass

try:
    import jupyternotify
    ip.register_magics(jupyternotify.JupyterNotifyMagics)
except ImportError:
    pass


@register_line_magic
def matplotlib_plt(line):
    ip.ex('import matplotlib')
    ip.ex('import matplotlib.pyplot as plt')
    ip.ex('print("Matplotlib", matplotlib.__version__)')


@register_line_magic
def pandas(line):
    ip.ex("""
try:
    import pandas as pd
except ImportError:
    pass
else:
    print("Pandas", pd.__version__)
""")


@register_line_magic
def numpy(line):
    """Import Numpy"""
    ip.ex('import numpy as np')
    ip.ex('from numpy import ma')
    ip.ex('print("Numpy", np.__version__)')


@register_line_magic
def astropy(line):
    """Import Astropy"""
    ip.run_line_magic('numpy', None)
    ip.ex('import astropy')
    ip.ex('print("Astropy", astropy.__version__)')
    ip.ex('import astropy.units as u')
    ip.ex('from astropy.coordinates import SkyCoord')
    ip.ex('from astropy.io import ascii, fits')
    ip.ex('from astropy.nddata import NDData, VarianceUncertainty')
    ip.ex('from astropy.table import Table, Column, MaskedColumn')
    ip.ex('from astropy.time import Time, TimeDelta')
    ip.ex('from astropy.wcs import WCS')


@register_line_magic
def mpdaf(line):
    """Import MPDAF"""
    ip.run_line_magic('astropy', None)
    ip.ex('import mpdaf')
    ip.ex('print("MPDAF", mpdaf.__version__)')
    ip.ex('from mpdaf.obj import Cube, Image, Spectrum, WCS, WaveCoord')
    ip.ex('from mpdaf.drs import PixTable')
    ip.ex('from mpdaf.sdetect import Source, Catalog')


@register_line_magic
def dragons(line):
    """Import dragons"""
    ip.run_line_magic('astropy', None)
    ip.ex('import astrodata')
    ip.ex('import gemini_instruments')
    ip.ex('print("astrodata", astrodata.__version__)')


@register_line_magic
def pydata(line):
    """Import MPDAF"""
    ip.run_line_magic('matplotlib', line)
    ip.run_line_magic('astropy', None)
    ip.run_line_magic('pandas', None)
    ip.run_line_magic('matplotlib_plt', None)


@register_line_magic
def imu(line):
    ip.ex('import ipython_memory_usage.ipython_memory_usage as imu')
    ip.ex('imu.start_watching_memory()')


# del numpy, astropy, mpdaf, imu
