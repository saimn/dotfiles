from __future__ import print_function

import site
from os import environ
from os.path import join
from sys import version_info

if 'VIRTUAL_ENV' in environ:
    virtual_env = join(environ.get('VIRTUAL_ENV'),
                       'lib',
                       'python{}.{}'.format(version_info.major,
                                            version_info.minor),
                       'site-packages')
    site.addsitedir(virtual_env)
    print('Using Virtualenv : ' + virtual_env)
    del virtual_env

del site, environ, join, version_info
