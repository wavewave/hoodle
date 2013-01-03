cabal install --enable-documentation 

sudo apt-get install cadaver

echo "login $SRVRID"$'\n'"password $SRVRPKEY" > $HOME/.netrc 
chmod 0600 $HOME/.netrc 

tar cvzf hoodle-types.tar.gz $HOME/.cabal/share/doc/hoodle-types*
echo "open $SRVR"$'\n'"put hoodle-types.tar.gz"$'\n'" "  > script 


#chmod 0600 whoisit


#scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i whoisit hoodle-types.tar.gz $SRVRID@$SRVR:$HOME/

cadaver < script  

rm script 
rm $HOME/.netrc 

