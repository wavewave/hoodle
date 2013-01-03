#!/bin/bash 

sudo apt-get install cadaver 
#libghc-hscolour-dev libghc-hstringtemplate-dev gtk2hs-buildtools libghc-gtk-dev libghc-gtk-doc 

cabal install transformers
cabal install hscolour 

mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install --force-reinstalls ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf

cabal install gtk2hs-buildtools

# for dep installation 
$HOME/.cabal/bin/build bootstrap --config=build.conf

# for documentation of dep packages
$HOME/.cabal/bin/build haddockboot --config=build.conf 

# for documentation of this package
cabal install  --enable-documentation
cabal haddock --hyperlink-source
cabal copy 

echo "machine $SRVR"'\n'"login $SRVRID"'\n'"password $SRVRPKEY" > $HOME/.netrc 
chmod 0600 $HOME/.netrc 

tar cvzf hoodle.tar.gz $HOME/.cabal/share/doc/hoodle* $HOME/.cabal/share/doc/xournal* $HOME/.cabal/share/doc/coroutine-object*
echo "open http://$SRVR:$SRVRPORT$SRVRDIR"'\n'"put hoodle.tar.gz"'\n'" "  > script 

cadaver < script  

rm script 
rm $HOME/.netrc 


# this is needed for checking
cabal install --enable-tests
