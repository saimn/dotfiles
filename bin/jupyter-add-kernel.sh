#!/bin/bash
# -*- coding: utf-8 -*-

pip install ipykernel
# TODO: detect conda vs pyenv
python -m ipykernel install --user --name=$PYENV_VERSION
