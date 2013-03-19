try:
    import line_profiler
except ImportError:
    line_profiler = False

def load_ipython_extension(ip):
    if line_profiler:
        ip.define_magic('lprun', line_profiler.magic_lprun)
