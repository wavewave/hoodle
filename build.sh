#!/bin/bash

pushd coroutine-object; cabal install ; popd 
pushd xournal-types ; cabal install ; popd 
pushd xournal-parser; cabal install ; popd
pushd hoodle-types ; cabal install ; popd 
pushd hoodle-parser ; cabal install ; popd 
pushd hoodle-builder ; cabal install ; popd
pushd hoodle-render ; cabal install ; popd 
pushd hoodle-core ; cabal install ; popd 
pushd hoodle ; cabal install ; popd 

