#!/bin/bash 

sudo apt-get install libghc-hstringtemplate-dev gtk2hs-buildtools libghc-gtk-dev libghc-gtk-doc 

mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install --force-reinstalls ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf

#cabal install gtk2hs-buildtools
$HOME/.cabal/bin/build bootstrap --config=build.conf

cabal install --enable-tests

