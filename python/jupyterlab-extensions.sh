#!/bin/sh

pip install -r ./jupyter-requirements.txt

jupyter labextension install @jupyter-widgets/jupyterlab-manager

jupyter labextension install @jupyterlab/github
jupyter labextension install @jupyterlab/shortcutui
jupyter labextension install @jupyterlab/toc
# jupyter labextension install jupyterlab_vim

jupyter labextension install @ijmbarr/jupyterlab_spellchecker
jupyter labextension install @mflevine/jupyterlab_html

# git
jupyter labextension install @jupyterlab/git
pip install jupyterlab-git
jupyter serverextension enable --py jupyterlab_git

# LaTeX
pip install jupyterlab_latex
jupyter labextension install @jupyterlab/latex

# Matplotlib
pip install ipympl
jupyter labextension install jupyter-matplotlib

# jupyterlab_code_formatter
jupyter labextension install @ryantam626/jupyterlab_code_formatter
pip install jupyterlab_code_formatter
jupyter serverextension enable --py jupyterlab_code_formatter

# bokeh
jupyter labextension install jupyterlab_bokeh
