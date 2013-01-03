cabal install 

echo "$SRVRPKEY" > whoisit 

tar cvzf doc.tar.gz $HOME/.cabal/doc
scp doc.tar.gz

scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i whoisit doc.tar.gz $SRVRID@$SRVR:$HOME/

rm whoisit 

