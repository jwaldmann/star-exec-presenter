#!/bin/bash

ARCH=x86_64-linux

# if you change this, be sure to change stack.yaml as well
# RESOLVER=lts-8.22
# GHC=8.0.2
RESOLVER=nightly-2017-08-15
GHC=8.2.1

APP=.stack-work/install/$ARCH/$RESOLVER/$GHC/bin/star-exec-presenter
KET=star-exec-presenter.keter

strip $APP
rm -fv $KET
tar -c --dereference --hard-dereference -z -v -f $KET $APP static config TPDB*.zip johannes_waldmann_tpdb-8.0.7_XML.zip
# scp $KET termcomp.imn.htwk-leipzig.de:/opt/keter/incoming
cp -v $KET /opt/keter/incoming
