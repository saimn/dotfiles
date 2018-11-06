#!/bin/bash
# -*- coding: utf-8 -*-

# Install Miniconda
wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash ./Miniconda3-latest-Linux-x86_64.sh -b

CONDA=$HOME/miniconda3/bin/conda
PIP=$HOME/miniconda3/bin/pip

$CONDA install -y \
    cython \
    flask \
    ipython \
    jinja2 \
    joblib \
    jupyter \
    line_profiler \
    matplotlib \
    memory_profiler \
    numexpr \
    numpy \
    numpydoc \
    pandas \
    pillow \
    psutil \
    pytest \
    pytest-cov \
    pyyaml \
    scikit image \
    scikit-learn \
    scipy \
    seaborn \
    snakeviz \
    sphinx \
    sphinx_rtd_theme \
    sqlalchemy

# $CONDA config --append channels openastronomy

# $CONDA install \
#     aplpy \
#     astroquery \
#     fitsio

# $PIP install -r ~/lib/dotfiles/python/conda-requirements.txt

# $PIP install git+https://github.com/ianozsvald/ipython_memory_usage.git\#egg\=ipython_memory_usage
