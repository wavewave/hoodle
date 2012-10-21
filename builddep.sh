#!/bin/bash 

cabal install transformers

ghc-pkg list

mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf

cabal install gtk2hs-buildtools
$HOME/.cabal/bin/build bootstrap --config=build.conf

cabal install --enable-tests

