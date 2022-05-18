{ pkgs ? import ./nix/pinnedNixpkgs.nix }:

with pkgs;

let
  deps = callPackage ./deps.nix { };
  hsconfig = callPackage ./config.nix {
    inherit deps;
    inherit haskell;
    #poppler = pkgs.poppler_gi;
    gtk3 = pkgs.gtk3;
    #inherit makeWrapper shared-mime-info gsettings-desktop-schemas;
  };
  newHsPkgs = haskell.packages.ghc8107.override { overrides = hsconfig; };
  #hoodleWrapper = stdenv.mkDerivation {
  #  pname = "hoodle-wrapper";
  #  version = "1.0";
  #  nativeBuildInputs = [ makeWrapper ];
  #  buildInputs = [ newHsPkgs.hoodle ];
  #  installPhase = ''
  #    mkdir -p $out/bin
  #    makeWrapper ${newHsPkgs.hoodle}/bin/hoodle $out/bin/hoodle
  #  '';
  #};
  # hoodleWrapper = mkbuildEnv {
  #   name = "hoodle-wrapper";
  #   buildInputs = [ pkgs.shared-mime-info ];
  #   paths = [ newHsPkgs.hoodle ]; # pkgs.shared-mime-info ];
  #   #pathsToLink =
  #   #  [ "/share/applications" "/share/mime" "/share/menus" "/share/icons" ];
  #   postBuild = ''
  #     mkdir -p $out/share
  #     cp -a ${pkgs.shared-mime-info}/share/* $out/share

  #     export XDG_DATA_DIRS=$out/share
  #     chmod -R u+w $out/share/mime
  #     update-mime-database -V $out/share/mime
  #     #if [ -w $out/share/applications ]; then
  #     #  update-desktop-database $out/share/applications
  #     #fi

  #     #if [ -w $out/share/mime ] \
  #     #   && [ -w $out/share/mime/packages ]; then
  #     #  
  #     #fi
  #  '';
  #};

in with newHsPkgs; {
  inherit microlens coroutine-object xournal-types xournal-parser hoodle-types
    hoodle-builder hoodle-parser hoodle-render hoodle-publish hoodle-core
    hoodle;
}
