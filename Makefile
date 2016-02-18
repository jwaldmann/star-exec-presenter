.PHONY: clean create-db deploy install run

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "		 clean            to clean up stack-work dir"
	@echo "		 create-db        to create a new PostgreSQL user and database"
	@echo "		 deploy           to create executable and move to correct place"
	@echo "		 install          to install star-exec-presenter"
	@echo "		 run              to start yesod devel with stack"


clean:
	rm stack.yaml
	rm -R .stack-work/

create-db:
	sudo -u postgres createuser -P yesod
	sudo -u postgres createdb -O yesod yesod


install:
	stack setup
	stack build yesod-bin-1.4.17.1 --verbosity silent
	stack build --verbosity silent


deploy:
	cabal clean
	cabal configure && cabal build
	sudo service star-exec-presenter stop
	sudo cp dist/build/star-exec-presenter/star-exec-presenter /var/star-exec-presenter
	sudo cp -R config/ /var/star-exec-presenter
	sudo cp -R static/ /var/star-exec-presenter
	sudo service star-exec-presenter start
	sudo service nginx start


run:
	stack exec yesod devel
