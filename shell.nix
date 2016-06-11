{ pkgs ? (import <nixpkgs>{}) }:

#with pkgs;

let hsconfig = self: super: {
     "poppler" = self.callPackage
     ({ mkDerivation, array, base, bytestring, cairo, containers
      , glib, gtk3, gtk3lib, gtk2hs-buildtools, mtl, pango, popplerlib
      , stdenv
      }:
      mkDerivation {
	pname = "poppler";
	version = "0.14.0";
	#src = fetchgit {
        #  url = "git://github.com/wavewave/poppler.git";
	#  rev = "021b41865672b10ed4f97e987afd0976b1587e29";
	#  sha256 = "00zv90vn1krva7jzjszjlbqypf4dzhrnvgbw6d1l071zxxipad8v";
        #};

        configureFlags = ["-fgtk3"];
	libraryHaskellDepends = [
	  array base bytestring cairo containers glib gtk3 mtl
	];
	libraryPkgconfigDepends = [ gtk3lib pango popplerlib ];
	libraryToolDepends = [ gtk2hs-buildtools ];
	homepage = "http://projects.haskell.org/gtk2hs";
	description = "Binding to the Poppler";
	license = stdenv.lib.licenses.gpl2;
      }) { popplerlib = pkgs.poppler; gtk3lib = pkgs.gtk3; };
    };

    newhaskellpackages = pkgs.haskellPackages.override { overrides = hsconfig; };
    hsenv = newhaskellpackages.ghcWithPackages 
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
          poppler
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
in pkgs.stdenv.mkDerivation { 
     name = "env-hoodle-build";
     propagatedBuildInputs = [ pkgs.wrapGAppsHook ] ;
     buildInputs = [ hsenv pkgs.x11 pkgs.xlibs.libXi pkgs.gtk3 pkgs.poppler pkgs.pkgconfig pkgs.sqlite pkgs.dbus_daemon
                     #gnome3.defaultIconTheme
                     #gnome3.gsettings_desktop_schemas
                   ];
     shellHook = ''
        $(grep export ${hsenv.outPath}/bin/ghc)
     '' + (if pkgs.stdenv.isDarwin then ''
        export NIX_LDFLAGS_AFTER+=" -L/usr/lib -F/Library/Frameworks -F/System/Library/Frameworks"
	export NIX_ENFORCE_PURITY=
     '' else "");

   }

#

        #export XDG_DATA_DIRS=${gtk3}/share/gsettings-schemas/${gtk3.name}/glib-2.0/schemas:${gnome3.gsettings_desktop_schemas}/share/gsettings-schemas/${gnome3.gsettings_desktop_schemas.name}:$XDG_DATA_DIRS
        #export XDG_DATA_DIRS=$GSETTINGS_SCHEMAS_PATH:$XDG_DATA_DIRS

#        export PATH=${pkgs.binutils}/bin:${pkgs.coreutils}/bin:$PATH
#        export NIX_STORE=$NIX_STORE
#        export LD_LIBRARY_PATH=${pkgs.xlibs.libX11}/lib:${pkgs.xlibs.libXi}/lib
#        export LIBRARY_PATH=${pkgs.xlibs.libX11}/lib:${pkgs.xlibs.libXi}/lib
#        export C_INCLUDE_PATH=${pkgs.xlibs.libX11}/include:${pkgs.xlibs.libXi}/include:${pkgs.xlibs.xproto}/include:${xlibs.inputproto}/include
