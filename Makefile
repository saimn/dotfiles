SHELL := /bin/bash


build/exa-linux-x86_64:
	cd build/ && \
		wget https://github.com/ogham/exa/releases/download/v0.8.0/exa-linux-x86_64-0.8.0.zip && \
		unzip exa-linux-x86_64-0.8.0.zip && \
		cd ..

bin/exa: build/exa-linux-x86_64
	ln -s ../build/exa-linux-x86_64 bin/exa

$$HOME/.pyenv:
	git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
	git clone https://github.com/yyuu/pyenv-ccache.git $(pyenv root)/plugins/pyenv-ccache
	git clone git://github.com/pyenv/pyenv-update.git $(pyenv root)/plugins/pyenv-update
