#!/bin/bash 

mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf

#cabal install gtk2hs-buildtools
sudo apt-get install gtk2hs-buildtools 
sudo apt-get install libghc-gtk-dev
sudo apt-get install libghc-gtk-doc 
$HOME/.cabal/bin/build bootstrap --config=build.conf

