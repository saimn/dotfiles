SHELL := /bin/bash

$$HOME/.pyenv:
	git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
	git clone https://github.com/yyuu/pyenv-ccache.git $(pyenv root)/plugins/pyenv-ccache
	git clone git://github.com/pyenv/pyenv-update.git $(pyenv root)/plugins/pyenv-update
