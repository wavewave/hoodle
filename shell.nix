{ pkgs ? import ./nix/pinnedNixpkgs.nix }:

with pkgs;

let
  deps = callPackage ./deps.nix { };
  hsconfig = callPackage ./config.nix {
    inherit deps;
    inherit haskell;
    poppler = pkgs.poppler;
    gtk3 = pkgs.gtk3;
  };
  newHsPkgs = haskell.packages.ghc865.override { overrides = hsconfig; };

  hsenv = newHsPkgs.ghcWithPackages (ps:
    with ps; [
      poppler
      case-insensitive
      attoparsec-conduit
      SHA
      aeson-pretty
      blaze-html
      hinotify
      zlib-conduit
      http-client-tls
      http-conduit
      xml
      fsnotify
      json
      pem
      pureMD5
      regex-base
      regex-posix
      configurator
      http-types
      system-filepath
      transformers-free
      websockets
      data-default-instances-containers
      asn1-parse
      Diff
      free
      strict
      microlens
      double-conversion
      uuid
      svgcairo
      monad-loops
      lens
      gd
      errors
      either
      pdf-toolbox-document
      directory-tree
      HTTP
      xml-types
      xml-conduit
      TypeCompose
      network-simple
    ]);

in mkShell {
  buildInputs = [
    hsenv
    cabal-install
    pkgconfig
    gdk-pixbuf
    gnome3.adwaita-icon-theme
  ]; # hicolor-icon-theme ];
  shellHook = ''
    export TESTDIR=${gnome3.adwaita-icon-theme}
    export XDG_DATA_DIRS=${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}:${gtk3}/share/gsettings-schemas/${gtk3.name}:$XDG_DATA_DIRS
  '';
}
