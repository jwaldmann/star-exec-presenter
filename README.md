

cabal sandbox init && cabal install --enable-tests . yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals && yesod devel
