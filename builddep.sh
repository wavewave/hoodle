#!/bin/bash 

sudo apt-get install cadaver 
#libghc-hscolour-dev libghc-hstringtemplate-dev gtk2hs-buildtools libghc-gtk-dev libghc-gtk-doc 

cabal install transformers
cabal install hscolour 

mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install --force-reinstalls ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf

#cabal install gtk2hs-buildtools
$HOME/.cabal/bin/build bootstrap --config=build.conf

# this is needed for checking
cabal install --enable-tests

$HOME/.cabal/bin/build haddockboot --config=build.conf 

echo "machine $SRVR"'\n'"login $SRVRID"'\n'"password $SRVRPKEY" > $HOME/.netrc 
chmod 0600 $HOME/.netrc 

tar cvzf hoodle.tar.gz $HOME/.cabal/share/doc/hoodle* $HOME/.cabal/share/doc/xournal* $HOME/.cabal/share/doc/coroutine-object*
echo "open http://$SRVR:$SRVRPORT$SRVRDIR"'\n'"put hoodle.tar.gz"'\n'" "  > script 

cadaver < script  

rm script 
rm $HOME/.netrc 


