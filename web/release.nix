{ pkgs ? import ../nix/pinnedNixpkgs.nix }:

with pkgs;

let

  newHsPkgs = haskell.packages.ghc865.override {
    overrides = self: super: {
      hoodle-server = self.callCabal2nix "hoodle-server" ./server {};
      hoodle-web-common = self.callCabal2nix "hoodle-web-common" ./common {};
    };
  };

in

with newHsPkgs;
{
  inherit hoodle-server hoodle-web-common;
}
