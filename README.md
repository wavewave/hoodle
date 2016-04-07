# hoodle
hoodle : A pen notetaking program written in haskell

Installation using stack
------------------------

1. Download and install stack: http://docs.haskellstack.org/en/stable/install_and_upgrade/#linux
   You could also use a distro-specific installation process or install it from source
2. Run git clone https://github.com/wavewave/hoodle.git
3. Run cd hoodle
4. Run stack build gtk2hs-buildtools
5. Run stack build
6. Wait, fingers crossed.

At some point, stack may ask you to let it install a fresh, local (to the user, not the project) version of GHC; I suggest you let it do so, although you can use the --use-system-ghc flag to avoid this.
