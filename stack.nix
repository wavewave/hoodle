{ ghc
, pkgs ? import <nixpkgs> {}
}:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "stack-hoodle";
  buildInputs = [
      pkgs.gtk3
      pkgs.poppler
      pkgs.gd
      pkgs.git
      pkgs.librsvg
  ];
}
