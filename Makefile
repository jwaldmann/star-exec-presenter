GHC_VERSION = 8.2.2
LTS_VERSION = 11.6

.PHONY: clean create-db deploy install run

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "		 clean            to clean up stack-work dir"
	@echo "		 create-db        to create a new PostgreSQL user and database"
	@echo "		 deploy           to create executable and move to correct place"
	@echo "		 install          to install star-exec-presenter"
	@echo "		 run              to start yesod devel with stack"


clean:
	# rm stack.yaml
	rm -R .stack-work/

create-db:
	sudo -u postgres createuser -P yesod
	sudo -u postgres createdb -O yesod yesod


install:
	stack setup
	# stack build yesod-bin --verbosity silent
	stack build --verbosity silent


deploy:
	stack build
	sudo service star-exec-presenter stop
	sudo cp .stack-work/install/x86_64-linux/lts-$(LTS_VERSION)/$(GHC_VERSION)/bin/star-exec-presenter /var/star-exec-presenter
	sudo cp -R config/ /var/star-exec-presenter
	sudo cp -R static/ /var/star-exec-presenter
	sudo service star-exec-presenter start
	sudo service nginx start


run:
	stack exec yesod devel
