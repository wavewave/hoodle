{ pkgs ? import ../nix/pinnedNixpkgs.nix }:

with pkgs;

let
  newhspkgs_ghcjs = haskell.packages.ghcjs.override {
    overrides = self: super: {
      coroutine-object = self.callCabal2nix "coroutine-object" ../coroutine-object {};
      hoodle-util = self.callCabal2nix "hoodle-util" ../util {};
      comonad = haskell.lib.dontCheck super.comonad;
      semigroupoids = haskell.lib.dontCheck super.semigroupoids;
      QuickCheck = haskell.lib.dontCheck super.QuickCheck;
      scientific = haskell.lib.dontCheck super.scientific;
      tasty-quickcheck = haskell.lib.dontCheck super.tasty-quickcheck;
      time-compat = haskell.lib.dontCheck super.time-compat;
    };
  };

  hsenv = haskellPackages.ghcWithPackages (p: with p; [
    acid-state
    microlens
    microlens-th
    monad-loops
    servant
    servant-server
    websockets
  ]);

  hsenv_ghcjs = newhspkgs_ghcjs.ghcWithPackages (p: with p; [
    coroutine-object
    ghcjs-base
    ghcjs-dom
    hoodle-util
    microlens
    microlens-th
    stm
  ]);

in
mkShell {
  name = "test-shell";
  buildInputs = [
    nodePackages.http-server
    cabal-install
    hsenv
    hsenv_ghcjs
    ormolu
  ];
}
