

cabal sandbox init && cabal install --enable-tests . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals && yesod devel

clear db: sudo -u postgres dropdb yesod
create db: sudo -u postgres createdb -O yesod yesod
