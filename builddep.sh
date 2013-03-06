cabal install --enable-documentation 

sudo apt-get install cadaver

tar cvzf hoodle-types.tar.gz $HOME/.cabal/share/doc/hoodle-types*

echo $CR | curl --digest -T hoodle-types.tar.gz -K - 



