#!/bin/bash 

sudo apt-get install libgd2-noxpm-dev libpoppler-glib-dev

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

tar cvzf hoodle.tar.gz $HOME/.cabal/share/doc/hoodle* $HOME/.cabal/share/doc/xournal* $HOME/.cabal/share/doc/coroutine-object*
echo $CR | curl --digest -T hoodle.tar.gz -K - $SRVRURL 

# this is needed for checking
cabal install --enable-tests
