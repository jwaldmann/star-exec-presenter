.PHONY: clean create-db deploy install

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "		 clean            to clean up cabal sandbox"
	@echo "		 create-db        to create a new PostgreSQL user and database"
	@echo "		 deploy           to create executable and move to correct place"
	@echo "		 install          to install star-exec-presenter"


clean:
	rm -R .cabal-sandbox
	rm cabal.sandbox.config


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
	sudo service star-exec-presenter stop
	sudo cp dist/build/star-exec-presenter/star-exec-presenter /var/star-exec-presenter
	sudo cp -R config/ /var/star-exec-presenter
	sudo cp -R static/ /var/star-exec-presenter
	sudo service star-exec-presenter start
	sudo service nginx start
