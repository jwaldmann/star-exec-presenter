#!/bin/bash

cabal exec -- ghci \
      -XCPP -XOverloadedStrings -XFlexibleContexts \
      -XTemplateHaskell -XQuasiQuotes -XGADTs \
      -XTypeFamilies -XMultiParamTypeClasses \
      -XGeneralizedNewtypeDeriving -XViewPatterns \
      $*
