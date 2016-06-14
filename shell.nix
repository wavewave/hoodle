{ pkgs ? (import <nixpkgs>{}) }:

let hsconfig = self: super: {
     "poppler" = self.callPackage
     ({ mkDerivation, array, base, bytestring, cairo, containers
      , glib, gtk3, gtk3lib, gtk2hs-buildtools, mtl, pango, popplerlib
      , stdenv
      }:
      mkDerivation {
        pname = "poppler";
        version = "0.14.0";

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
      
      "svgcairo" = self.callPackage
      ({ mkDerivation, base, cairo, glib, gtk2hs-buildtools, librsvg, mtl
       , stdenv, text
       }:
       mkDerivation {
         pname = "svgcairo";
         version = "0.13.0.4";
         libraryHaskellDepends = [ base cairo glib mtl text ];
         libraryPkgconfigDepends = [ librsvg ];
         libraryToolDepends = [ gtk2hs-buildtools ];
         homepage = "http://projects.haskell.org/gtk2hs/";
         description = "Binding to the libsvg-cairo library";
         license = stdenv.lib.licenses.bsd3;
       }) { };
       
      "gtk3"              = self.gtk3_0_14_1;
      "gtk2hs-buildtools" = self.gtk2hs-buildtools_0_13_0_5;
      "cairo"             = self.cairo_0_13_1_1;
      "glib"              = self.glib_0_13_2_2;
      "gio"               = self.gio_0_13_1_1;
      "pango"             = self.pango_0_13_1_1;
    };

    newhaskellpackages = pkgs.haskell.packages.ghc7103.override { overrides = hsconfig; };
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
          #esqueleto
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
     buildInputs = [ hsenv pkgs.x11 pkgs.xlibs.libXi pkgs.gtk3 pkgs.poppler pkgs.pkgconfig
                     pkgs.sqlite pkgs.dbus_daemon
                     pkgs.fontconfig
                     #pkgs.valgrind
                     pkgs.gnome3.defaultIconTheme
                     pkgs.gnome3.gsettings_desktop_schemas
                     pkgs.dejavu_fonts
                   ];
     shellHook = ''
       $(grep export ${hsenv.outPath}/bin/ghc)
       export GTK3PATH=${pkgs.gtk3}
       #export FONTCONFIG_PATH=${pkgs.fontconfig}
       export DEJAVU=${pkgs.dejavu_fonts}
       cat > fonts.conf <<EOF 
       <?xml version="1.0"?>
       <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
       <fontconfig>
         <dir>${pkgs.dejavu_fonts}/share/fonts/truetype</dir>
         <!--
           Accept deprecated 'mono' alias, replacing it with 'monospace'
         -->
         <match target="pattern">
           <test qual="any" name="family">
             <string>mono</string>
           </test>
           <edit name="family" mode="assign">
             <string>monospace</string>
           </edit>
         </match>
         <!--
           Accept alternate 'sans serif' spelling, replacing it with 'sans-serif'
         -->
         <match target="pattern">
           <test qual="any" name="family">
             <string>sans serif</string>
           </test>
           <edit name="family" mode="assign">
             <string>sans-serif</string>
           </edit>
         </match>
         <!--
           Accept deprecated 'sans' alias, replacing it with 'sans-serif'
         -->
         <match target="pattern">
           <test qual="any" name="family">
             <string>sans</string>
           </test>
           <edit name="family" mode="assign">
             <string>sans-serif</string>
           </edit>
         </match>
                                                   
         <alias>
           <family>DejaVu Serif</family>
         </alias>
         <alias>
           <family>DejaVu Sans</family>
         </alias>
         <alias>
           <family>DejaVu Sans Mono</family>
         </alias>
       </fontconfig>
       EOF

       export XDG_DATA_DIRS=${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}/glib-2.0/schemas:${pkgs.gnome3.gsettings_desktop_schemas}/share/gsettings-schemas/${pkgs.gnome3.gsettings_desktop_schemas.name}:$XDG_DATA_DIRS
       export XDG_DATA_DIRS=$GSETTINGS_SCHEMAS_PATH:$XDG_DATA_DIRS      
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
