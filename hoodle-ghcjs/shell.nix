{ pkgs ?
  import (builtins.fetchTarball {
    name = "nixos-20.03";
    url = "https://github.com/nixos/nixpkgs/archive/5272327b81ed355bbed5659b8d303cf2979b6953.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {}
}:

with pkgs;

let
  newhspkgs = haskell.packages.ghcjs.override {
    overrides = self: super: {
      QuickCheck = haskell.lib.dontCheck super.QuickCheck;
      scientific = haskell.lib.dontCheck super.scientific;
      tasty-quickcheck = haskell.lib.dontCheck super.tasty-quickcheck;
      time-compat = haskell.lib.dontCheck super.time-compat;
    };
  };
  hsenv = newhspkgs.ghcWithPackages (p: with p; [
    ghcjs-base
    stm
  ]);

in
mkShell {
  name = "test-shell";
  buildInputs = [
    nodejs
    hsenv
  ];
}
