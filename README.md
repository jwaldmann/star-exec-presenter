

cabal sandbox init && cabal install --enable-tests . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals && yesod devel

* create user: sudo -u postgres createuser -P yesod
* clear db: sudo -u postgres dropdb yesod
* create db: sudo -u postgres createdb -O yesod yesod
