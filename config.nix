{ lib, deps, haskell, poppler_gi, gtk3, makeWrapper, shared-mime-info
, gsettings-desktop-schemas, dconf, gdk-pixbuf }:

self: super: {

  "pdf-toolbox-core" =
    self.callCabal2nix "pdf-toolbox-core" (deps.pdf-toolbox + "/core") { };
  "pdf-toolbox-content" =
    self.callCabal2nix "pdf-toolbox-content" (deps.pdf-toolbox + "/content")
    { };
  "pdf-toolbox-document" =
    self.callCabal2nix "pdf-toolbox-document" (deps.pdf-toolbox + "/document")
    { };

  "poppler" = self.callPackage ({ lib, mkDerivation, array, base, bytestring
    , cairo, containers, glib, gtk3, gtk3lib, gtk2hs-buildtools, mtl, pango
    , popplerlib }:
    mkDerivation {
      pname = "poppler";
      version = "0.15";
      src = deps.poppler;

      configureFlags = [ "-fgtk3" ];
      hardeningDisable = [ "fortify" ];
      libraryHaskellDepends =
        [ array base bytestring cairo containers glib gtk3 mtl ];
      libraryPkgconfigDepends = [ gtk3lib pango popplerlib ];
      libraryToolDepends = [ gtk2hs-buildtools ];
      homepage = "http://projects.haskell.org/gtk2hs";
      description = "Binding to the Poppler";
      license = lib.licenses.gpl2;
    }) {
      popplerlib = poppler_gi;
      gtk3lib = gtk3;
    };

  coroutine-object =
    self.callCabal2nix "coroutine-object" ./coroutine-object { };

  xournal-types = self.callCabal2nix "xournal-types" ./xournal-types { };

  xournal-parser = self.callCabal2nix "xournal-parser" ./xournal-parser { };

  hoodle-types = self.callCabal2nix "hoodle-types" ./types { };

  hoodle-builder = self.callCabal2nix "hoodle-builder" ./builder { };

  hoodle-parser = self.callCabal2nix "hoodle-parser" ./parser { };

  hoodle-util = self.callCabal2nix "hoodle-util" ./util { };

  hoodle-render = self.callCabal2nix "hoodle-render" ./render { };

  hoodle-publish = self.callCabal2nix "hoodle-publish" ./publish { };

  hoodle-core = self.callCabal2nix "hoodle-core" ./core { };

  hoodle = (self.callCabal2nix "hoodle" ./hoodle { }).overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs
      ++ [ makeWrapper gsettings-desktop-schemas gdk-pixbuf ];
    preFixup = ''
      wrapProgram "$out/bin/hoodle" \
       --prefix GIO_EXTRA_MODULES : "${lib.getLib dconf}/lib/gio/modules" \
       --prefix XDG_DATA_DIRS : "${shared-mime-info}/share" \
       --prefix XDG_DATA_DIRS : "${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}" \
       --prefix XDG_DATA_DIRS : "${gtk3}/share/gsettings-schemas/${gtk3.name}"
    '';
  });

}

#         --prefix XDG_DATA_DIRS : "$XDG_ICON_DIRS" \
#       --prefix XDG_DATA_DIRS : "$GSETTINGS_SCHEMAS_PATH"
