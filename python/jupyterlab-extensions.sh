#!/bin/sh

pip install -r ./jupyter-requirements.txt

# jupyter labextension install @jupyter-widgets/jupyterlab-manager
# jupyter labextension install @jupyterlab/github
# # jupyter labextension install @jupyterlab/shortcutui
# jupyter labextension install @jupyterlab/toc
# # jupyter labextension install jupyterlab_vim
# jupyter labextension install @ijmbarr/jupyterlab_spellchecker
# jupyter labextension install @mflevine/jupyterlab_html

# git
pip install jupyterlab-git

# LaTeX
# pip install jupyterlab_latex
# jupyter labextension install @jupyterlab/latex

# Matplotlib
# pip install ipympl

pip install jupyterlab_code_formatter
pip install jupyterlab_execute_time
pip install jupyter-resource-usage

# bokeh
# jupyter labextension install jupyterlab_bokeh
