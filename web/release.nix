{ pkgs ? import ../nix/pinnedNixpkgs.nix }:

with pkgs;

let

  newHsPkgs = haskell.packages.ghc865.override {
    overrides = self: super: {
      hoodle-server = self.callCabal2nix "hoodle-server" ./server {};
      hoodle-web-common = self.callCabal2nix "hoodle-web-common" ./common {};
    };
  };

  newHsPkgs_ghcjs = haskell.packages.ghcjs.override {
    overrides = self: super: {
      coroutine-object = self.callCabal2nix "coroutine-object" ../coroutine-object {};
      hoodle-util = self.callCabal2nix "hoodle-util" ../util {};
      comonad = haskell.lib.dontCheck super.comonad;
      semigroupoids = haskell.lib.dontCheck super.semigroupoids;
      QuickCheck = haskell.lib.dontCheck super.QuickCheck;
      scientific = haskell.lib.dontCheck super.scientific;
      tasty-quickcheck = haskell.lib.dontCheck super.tasty-quickcheck;
      time-compat = haskell.lib.dontCheck super.time-compat;
      hoodle-web-common = self.callCabal2nix "hoodle-web-common" ./common {};
      hoodle-web = self.callCabal2nix "hoodle-web" ./client {};
    };
  };

  hoodle-static = stdenv.mkDerivation {
    name = "hoodle-static";
    src = ./static;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p $out
      cp hoodle.html pen.js svg.min.js svg.min.js.map $out
      cp -a ${newHsPkgs_ghcjs.hoodle-web}/bin/hoodle-web.jsexe $out
    '';
  };

in

with newHsPkgs;
{
  hoodle-server = newHsPkgs.hoodle-server;
  inherit hoodle-static;
  hoodle-web = newHsPkgs_ghcjs.hoodle-web;

}
