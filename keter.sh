#!/bin/bash

ARCH=x86_64-linux

# if you change this, be sure to change stack.yaml and config/keter.yml as well

# RESOLVER=lts-13.16
# GHC=8.6.4

APP=star-exec-presenter
KET=star-exec-presenter.keter

cp -v $(which $APP) bin/$APP
strip bin/$APP
rm -fv $KET
tar -c --dereference --hard-dereference -z -v -f $KET bin/$APP static config TPDB-*_XML.zip johannes_waldmann_tpdb-8.0.7_XML.zip  mario_wenzel_XML.zip termcomp2019_XML.zip termcomp2020_XML.zip
# scp $KET termcomp.imn.htwk-leipzig.de:/opt/keter/incoming
cp -v $KET /opt/keter/incoming
