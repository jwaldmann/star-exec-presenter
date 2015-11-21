A Web Application for Displaying Results of the Termination Competition
-----------------------

The Intl. Termination Competition 2014 <http://termination-portal.org/wiki/Termination_Competition_2014> will be run on <http://www.starexec.org> . 

Our Web App will provide these extra features:

* computation and display of results per category, 
  and medalists per meta-category
* flexible 2-dimensional results tables 
  (x-axis: solver, y-axis: benchmark) 
  (with filters for rows and columns, sorters, etc.)
* caching (and permanent storage) of results 
  (we intend to include also results of earlier competitions)

1 Getting source
----------------
There may be submodules, so:

    git clone git@github.com:rm--/star-exec-presenter.git


2 Installation
--------------
    cabal sandbox init
    cabal install alex happy yesod-bin
    cabal install --enable-tests .

You also need [Graphviz](http://www.graphviz.org/Download.php) on your system.

3 Database
----------
  create user:

    sudo -u postgres createuser -P yesod

  create db:

    sudo -u postgres createdb -O yesod yesod


4 Credentials
-------------

Please create the file '.star_exec' in your home directory with your [starexec account creadentials](https://www.starexec.org/starexec/secure/index.jsp).
The file must contain the following line:

    Login ">login<" ">password<"


5 Usage
-------

    yesod devel


Notes
-----

clear db:

    sudo -u postgres dropdb yesod
access db:

    sudo -u postgres psql -d yesod
