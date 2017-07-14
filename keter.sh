#!/bin/bash

ARCH=x86_64-linux
RESOLVER=lts-8.22
GHC=8.0.2

APP=.stack-work/install/$ARCH/$RESOLVER/$GHC/bin/star-exec-presenter
KET=star-exec-presenter.keter

strip $APP
rm -fv $KET
tar -c --dereference --hard-dereference -z -v -f $KET $APP static config TPDB*.zip johannes_waldmann_tpdb-8.0.7_XML.zip
# scp $KET termcomp.imn.htwk-leipzig.de:/opt/keter/incoming
cp -v $KET /opt/keter/incoming
