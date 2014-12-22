#!/bin/bash

# our use cabal-meta
#
#    cabal-meta install
cat sources.txt | xargs cabal install --force-reinstalls -fdyre -fhub -j 

