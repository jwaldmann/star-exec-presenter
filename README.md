A Web Application for Displaying Results of the Termination Competition
-----------------------
[![Circle CI](https://circleci.com/gh/rm--/star-exec-presenter.svg?style=svg)](https://circleci.com/gh/rm--/star-exec-presenter)

The Intl. Termination Competition 2014 <http://termination-portal.org/wiki/Termination_Competition_2014> will be run on <http://www.starexec.org> .

Our Web App will provide these extra features:

* computation and display of results per category,
  and medalists per meta-category
* flexible 2-dimensional results tables
  (x-axis: solver, y-axis: benchmark)
  (with filters for rows and columns, sorters, etc.)
* caching (and permanent storage) of results
  (we intend to include also results of earlier competitions)

This branch is running at the moment [here](http://termcomp.imn.htwk-leipzig.de/).

1 Getting source
----------------

    git clone git@github.com:rm--/star-exec-presenter.git


2 Installation
--------------

    make install

You also need [Graphviz](http://www.graphviz.org/Download.php) on your system.

3 Create database user and database:
------------------------------------

    make create-db

4 Credentials
-------------

Please create the file '.star_exec' in your home directory with your [starexec account credentials](https://www.starexec.org/starexec/secure/index.jsp).
This file must contain the following line:

    Login ">login<" ">password<"


5 Usage
-------

    make run


Notes
-----

clear db:

    sudo -u postgres dropdb yesod
access db:

    sudo -u postgres psql -d yesod

how to build documentation (locally)

    stack haddock --haddock-arguments="--hyperlinked-source --ignore-all-exports" --haddock-deps

