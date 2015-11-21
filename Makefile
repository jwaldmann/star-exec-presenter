.PHONY: install

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "		  install          install star-exec-presenter"

install:
	cabal sandbox init
	cabal sandbox add-source graphviz
	cabal install alex happy yesod-bin
	cabal install --enable-tests .
