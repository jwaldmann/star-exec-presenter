.PHONY: create-db, install

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "		 install          to install star-exec-presenter"
	@echo "		 create-db        to create a new PostgreSQL user and database"
	@echo "		 deploy           to create executable and move to correct place"


create-db:
	sudo -u postgres createuser -P yesod
	sudo -u postgres createdb -O yesod yesod


install:
	cabal sandbox init
	cabal update
	cabal install alex happy yesod-bin
	cabal install --enable-tests .


deploy:
	cabal clean
	cabal configure && cabal build
	cp dist/build/star-exec-presenter/star-exec-presenter /var/star-exec-presenter
	cp -R config/ /var/star-exec-presenter
	cp -R static/ /var/star-exec-presenter
