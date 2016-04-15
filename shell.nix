{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let hsenv = haskellPackages.ghcWithPackages 
      (p: with p; [ 
          cabal-install
          Diff
          HTTP
          TypeCompose
          aeson
          aeson-pretty
          attoparsec
          attoparsec-conduit
          base16-bytestring
          base64-bytestring
          blaze-builder-conduit
          blaze-markup
          blaze-html
          cairo
          cereal
          configurator
          crypto-api
          data-default
          data-default-instances-dlist
          dbus
          directory-tree
          double-conversion
          dyre
          either
          entropy
          errors
          esqueleto
          failure
          fsnotify
          gd
          gtk3
          handa-gdata
          http-client-conduit
          http-conduit
          io-streams
          lens
          mmorph
          monad-loops
          mtl
          network-info
          network-simple
          pango
          pdf-toolbox-core
          pdf-toolbox-content
          pdf-toolbox-document
          persistent
          persistent-sqlite
          persistent-template
          #poppler
          pureMD5
          safecopy
          stm
          strict
          svgcairo
          system-filepath
          transformers-free
          uuid
          websockets
          xml-conduit
          zlib-bindings
          zlib-conduit
          #
          HStringTemplate
          fgl
          filemanip
          hslogger
          regex-base regex-posix regex-compat MissingH
          gtk2hs-buildtools
    ]);
in stdenv.mkDerivation { 
     name = "env-hoodle-build";
     buildInputs = [ hsenv x11 xlibs.libXi gtk3 poppler pkgconfig sqlite ];
     shellHook = ''
        $(grep export ${hsenv.outPath}/bin/ghc)
        export PATH=${binutils}/bin:${coreutils}/bin:$PATH
        export NIX_STORE=$NIX_STORE
        export LD_LIBRARY_PATH=${xlibs.libX11}/lib:${xlibs.libXi}/lib
        export LIBRARY_PATH=${xlibs.libX11}/lib:${xlibs.libXi}/lib
        export C_INCLUDE_PATH=${xlibs.libX11}/include:${xlibs.libXi}/include:${xlibs.xproto}/include:${xlibs.inputproto}/include
     '';
   }
