SHELL := /bin/bash

EXA_VERSION="0.8.0"
FD_VERSION="6.3.0"

bin/exa:
	cd build/ && \
		wget https://github.com/ogham/exa/releases/download/v0.8.0/exa-linux-x86_64-0.8.0.zip && \
		unzip exa-linux-x86_64-0.8.0.zip && \
		cd ..
	ln -s ../build/exa-linux-x86_64 bin/exa

bin/fd:
	cd build/ && \
		wget https://github.com/sharkdp/fd/releases/download/v6.3.0/fd-v6.3.0-x86_64-unknown-linux-musl.tar.gz && \
		tar xf fd-v6.3.0-x86_64-unknown-linux-musl.tar.gz && \
		cd ..
	ln -s ../build/fd-v6.3.0-x86_64-unknown-linux-musl/fd bin/fd

$$HOME/.pyenv:
	git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
	git clone https://github.com/yyuu/pyenv-ccache.git $(pyenv root)/plugins/pyenv-ccache
	git clone git://github.com/pyenv/pyenv-update.git $(pyenv root)/plugins/pyenv-update
