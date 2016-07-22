{ poppler, gtk3 }:

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
      }) { popplerlib = poppler; gtk3lib = gtk3; };

      coroutine-object = self.callPackage ./coroutine-object {};

      xournal-types    = self.callPackage ./xournal-types {};

      xournal-parser   = self.callPackage ./xournal-parser { 
                           xournal-types = self.xournal-types; 
                         };

      hoodle-types     = self.callPackage ./hoodle-types {};

      hoodle-builder   = self.callPackage ./hoodle-builder { 
                           hoodle-types = self.hoodle-types; 
                         };

      hoodle-parser    = self.callPackage ./hoodle-parser { 
                           hoodle-types = self.hoodle-types; 
                           xournal-types = self.xournal-types; 
                         };

      hoodle-render    = self.callPackage ./hoodle-render { 
                           hoodle-types = self.hoodle-types; 
                         };

      hoodle-publish   = self.callPackage ./hoodle-publish { 
                           hoodle-types = self.hoodle-types; 
                           hoodle-render = self.hoodle-render; 
                         };

      hoodle-core      = self.callPackage ./hoodle-core { 
                           coroutine-object = self.coroutine-object;
                           hoodle-types = self.hoodle-types; 
                           hoodle-parser = self.hoodle-parser; 
                           hoodle-builder = self.hoodle-builder;
                           hoodle-render = self.hoodle-render; 
                           hoodle-publish = self.hoodle-publish;
                           xournal-parser = self.xournal-parser;
                           gtk3 = gtk3;
                         };
      hoodle           = self.callPackage ./hoodle { 
                           hoodle-core = self.hoodle-core; 
                         };
    };

in hsconfig

#    newHaskellPackages = pkgs.haskell.packages.ghc7103.override { overrides = hsconfig; };
#    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [ hoodle ]);
#in pkgs.stdenv.mkDerivation {
#     name = "testenv";
#     #propagatedBuildInputs = [ pkgs.wrapGAppsHook ] ;
#     buildInputs = [ hsenv ];
#   }