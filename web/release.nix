{ pkgs ? import ../nix/pinnedNixpkgs.nix }:

with pkgs;

let

  newHsPkgs = haskell.packages.ghc865.override {
    overrides = self: super: {
      hoodle-server = self.callCabal2nix "hoodle-server" ./server {};
      hoodle-web-common = self.callCabal2nix "hoodle-web-common" ./common {};
    };
  };

  hoodle-static = ./static;

in

with newHsPkgs;
{
  inherit hoodle-server hoodle-web-common;
  inherit hoodle-static;
}
