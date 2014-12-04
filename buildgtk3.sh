#!/bin/bash

# our use cabal-meta
#
#    cabal-meta install
cat sources.txt | xargs cabal install --constraint="gtk > 1.0"  -fgtk3 --force-reinstalls -j  -v

