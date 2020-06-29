#!/bin/bash 

#cabal install gtk2hs-buildtools
sudo apt-get install libgd2-xpm-dev libghc-hstringtemplate-dev gtk2hs-buildtools libghc-gtk-dev libghc-gtk-doc 


mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf

$HOME/.cabal/bin/build bootstrap --config=build.conf

