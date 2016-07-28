{ pkgs, poppler, gtk3 }:

with import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; };

self: super:

let
    srcurl = x: with x; {
      src = pkgs.fetchurl {
        url = "mirror://hackage/${pname}-${version}.tar.gz";
        inherit sha256;
      };
    }; 

    overrideCabal2 = p: q: r: overrideCabal (self.callPackage p q) (drv: r // srcurl r);

#(with x; { src = pkgs.fetchurl { url = "mirror://hackage/${pname}-${version}.tar.gz";
#                                   inherit sha256; }; })
#}
#    );

in

{
   "gtk2hs-buildtools" = self.callPackage
    ({ mkDerivation, alex, array, base, Cabal,
       containers, directory
     , filepath, happy, hashtables, pretty, process, random, stdenv
     }:
     mkDerivation {
       pname = "gtk2hs-buildtools";
       version = "0.13.2.1";
       sha256 = "86ea21a2e07d83dcff57eb224a7c76835fb4ea561e4d6ba9b52b8035b38d064b";
       isLibrary = true;
       isExecutable = true;
       libraryHaskellDepends = [
         array base #Cabal
         containers directory filepath hashtables pretty
         process random
       ];
       libraryToolDepends = [ alex happy ];
       executableHaskellDepends = [ base ];
       homepage = "http://projects.haskell.org/gtk2hs/";
       description = "Tools to build the Gtk2Hs suite of User Interface libraries";
       license = stdenv.lib.licenses.gpl2;
     }) {};
  


  "poppler" = self.callPackage
  ({ mkDerivation, array, base, bytestring, cairo, containers
   , glib, gtk3, gtk3lib, gtk2hs-buildtools, mtl, pango, popplerlib
   , stdenv
   }:
    mkDerivation {
    pname = "poppler";
    version = "0.14.1";

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

  coroutine-object = overrideCabal2 ./coroutine-object {} {
                       pname = "coroutine-object";
                       version = "0.3.0";
                       sha256 = "1hgpy3fswhars994mz3756firiy0g5brx7w9is4nfhg8mr5vf3yg";
                     };



  xournal-types    = self.callPackage ./xournal-types {};

  xournal-parser   = self.callPackage ./xournal-parser { 
                       xournal-types = self.xournal-types; 
                     };

  hoodle-types     = overrideCabal2 ./hoodle-types {} {
    pname = "hoodle-types";
    version = "0.4";
    sha256 = "1dikxb2xx2a4fn6abs1pwbl2xy7ld7b1jrx38gphpbnbf9hkbcqx";
  };

  hoodle-builder   = overrideCabal2 ./hoodle-builder {
    hoodle-types = self.hoodle-types; 
  } {
    pname = "hoodle-builder";
    version = "0.4";
    sha256 = "015cwdjznil7hwasmvcfh1r5jvsaarc8f8mxsn8241fn688fyl7k";
  };

  hoodle-parser    = overrideCabal2 ./hoodle-parser { 
    hoodle-types = self.hoodle-types; 
    xournal-types = self.xournal-types; 
  } {
    pname = "hoodle-parser";
    version = "0.4";
    src = null;
    sha256 = "0yjii2s58p7vcxrir1i49qy1hfn3j5y5v8x56fl3asnnqkj0b31f";
  };

  hoodle-render    = overrideCabal2 ./hoodle-render {
    hoodle-types = self.hoodle-types;
  } {
    pname = "hoodle-render";
    version = "0.6";
    sha256="1qyld0vmp89x7awnbi3iqchh0f90hi7vqdxdg0my4s3xrq50y1s7";
  };

  hoodle-publish   = overrideCabal2 ./hoodle-publish { 
    hoodle-types = self.hoodle-types; 
    hoodle-render = self.hoodle-render;
  } {
    pname = "hoodle-publish";
    version = "0.2.1";
    sha256 = "1gqkfvay3jgdhx250ja8yx6nsv78jj55qd0qf0dbd4mblfrpxrsx";
  };

  hoodle-core      = overrideCabal2 ./hoodle-core { 
    coroutine-object = self.coroutine-object;
    hoodle-types = self.hoodle-types; 
    hoodle-parser = self.hoodle-parser; 
    hoodle-builder = self.hoodle-builder;
    hoodle-render = self.hoodle-render; 
    hoodle-publish = self.hoodle-publish;
    xournal-parser = self.xournal-parser;
    gtk3 = gtk3;
  } {
    pname = "hoodle-core";
    version = "0.16.0";
    sha256 = "1v1y99x5rbkn85f91pdw19jfccwhcyfklg1qli0d7lq2c6aak4ka";
  };
  
  hoodle           = overrideCabal2 ./hoodle { 
    hoodle-core = self.hoodle-core;
  } {
    pname = "hoodle";
    version = "0.5";
    sha256 = "1rhxmiqwmzmnaqw7qj77k9y8svyy0gknpn8di7q5r9w1bdl807q5";
  };
}


