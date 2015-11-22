.PHONY: create-db, install

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "		 install          to install star-exec-presenter"
	@echo "		 create-db        to create a new PostgreSQL user and database"


create-db:
	sudo -u postgres createuser -P yesod
	sudo -u postgres createdb -O yesod yesod


install:
	cabal sandbox init
	cabal update
	cabal install alex happy yesod-bin
	cabal install --enable-tests .
