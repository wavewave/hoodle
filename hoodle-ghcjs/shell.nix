{ pkgs ?
  import (builtins.fetchTarball {
    name = "nixos-20.03";
    url = "https://github.com/nixos/nixpkgs/archive/5272327b81ed355bbed5659b8d303cf2979b6953.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {}
}:

with pkgs;

let
  newhspkgs_ghcjs = haskell.packages.ghcjs.override {
    overrides = self: super: {
      coroutine-object = self.callCabal2nix "coroutine-object" ../coroutine-object {};
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
    monad-loops
    servant
    servant-server
    websockets
  ]);

  hsenv_ghcjs = newhspkgs_ghcjs.ghcWithPackages (p: with p; [
    coroutine-object
    ghcjs-base
    stm
  ]);

in
mkShell {
  name = "test-shell";
  buildInputs = [
    nodejs
    hsenv
    hsenv_ghcjs
    ormolu
  ];
}
