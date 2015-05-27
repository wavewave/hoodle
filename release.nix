{ pkgs ? import <nixpkgs> {} }:

let stdenv = pkgs.stdenv;
    newghcpkgs = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules> {
      ghc = pkgs.haskell.compiler.ghc7101;
      packageSetConfig = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules/configuration-ghc-7.10.x.nix> {};
      overrides = self: super: rec { 
        poppler = self.callPackage (
          { mkDerivation, array, base, bytestring, cairo, containers
          , gdk_pixbuf, glib, gtk, gtk2hs-buildtools, mtl, pango }:
          mkDerivation {
            pname = "poppler";
            version = "0.13.1";
            src = pkgs.fetchgit { url = "https://github.com/wavewave/poppler.git";
                                  rev = "05123d9f054a353ea0f06a3179f3f2c773594079";
                                  sha256 = "6e142219c94c9375f54c55825ab952fa0c1d4358b2706fad63f18885ebf77a40"; 
                                };
            buildDepends = [
              array base bytestring cairo containers glib gtk mtl
            ];
            buildTools = [ gtk2hs-buildtools ];
            pkgconfigDepends = [ pkgs.gtk pkgs.glib pkgs.gdk_pixbuf pkgs.pango pkgs.poppler ];
            homepage = "http://projects.haskell.org/gtk2hs";
            description = "Binding to the Poppler";
            license = stdenv.lib.licenses.gpl2;
          }) {};
        coroutine-object = self.callPackage ./coroutine-object {};
        xournal-types = self.callPackage ./xournal-types {};
        xournal-parser = self.callPackage ./xournal-parser { inherit xournal-types; };
        hoodle-types = self.callPackage ./hoodle-types {};
        hoodle-builder = self.callPackage ./hoodle-builder { inherit hoodle-types; };
        hoodle-parser = self.callPackage ./hoodle-parser { inherit hoodle-types xournal-types; };
        hoodle-render = self.callPackage ./hoodle-render { inherit hoodle-types; };
        hoodle-publish = self.callPackage ./hoodle-publish { inherit hoodle-types hoodle-render; };
        hoodle-core = self.callPackage ./hoodle-core { inherit coroutine-object hoodle-types hoodle-parser hoodle-builder hoodle-render hoodle-publish xournal-parser; };
        hoodle = self.callPackage ./hoodle { inherit hoodle-core; };
      };
    };
in with newghcpkgs; { 
     inherit coroutine-object xournal-types xournal-parser hoodle-types hoodle-builder hoodle-parser hoodle-render hoodle-publish hoodle-core hoodle;
   }
