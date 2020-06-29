{ deps, haskell, poppler, gtk3 }:

self: super: {

  "pdf-toolbox-core" = self.callCabal2nix "pdf-toolbox-core" (deps.pdf-toolbox + "/core") {};
  "pdf-toolbox-content" = self.callCabal2nix "pdf-toolbox-content" (deps.pdf-toolbox + "/content") {};
  "pdf-toolbox-document" = self.callCabal2nix "pdf-toolbox-document" (deps.pdf-toolbox + "/document") {};

   "poppler" =
     self.callPackage
       ({ mkDerivation, array, base, bytestring, cairo, containers
        , glib, gtk3, gtk3lib, gtk2hs-buildtools, mtl, pango, popplerlib
        , stdenv
        }:
        mkDerivation {
         pname = "poppler";
         version = "0.15";
         src = deps.poppler;

         configureFlags = ["-fgtk3"];
         hardeningDisable=["fortify"];
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

  hoodle-types     = self.callPackage ./types {};

  hoodle-builder   = self.callPackage ./builder {
                       hoodle-types = self.hoodle-types;
                     };

  hoodle-parser    = self.callPackage ./parser {
                       hoodle-types = self.hoodle-types;
                       xournal-types = self.xournal-types;
                     };

  hoodle-util      = self.callPackage ./util {};

  hoodle-render    = self.callPackage ./render {
                       hoodle-types = self.hoodle-types;
                     };

  hoodle-publish   = self.callPackage ./publish {
                       hoodle-types = self.hoodle-types;
                       hoodle-render = self.hoodle-render;
                     };

  hoodle-core      = self.callPackage ./core {
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



}
