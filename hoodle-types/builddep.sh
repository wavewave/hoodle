cabal install --enable-documentation 

tar cvzf hoodle-types.tar.gz $HOME/.cabal/share/doc/hoodle-types*

echo $CR | curl --digest -T hoodle-types.tar.gz -K - $SRVRURL 




