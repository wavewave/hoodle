{
  description = "Hoodle: pen notetaking program";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;

        parseCabalProject = import ./parse-cabal-project.nix;
        hoodlePackages = parseCabalProject ./cabal.project;
        hoodlePackageNames = builtins.map ({ name, ... }: name) hoodlePackages;
        haskellOverlay = self: super:
          builtins.listToAttrs (builtins.map ({ name, path }: {
            inherit name;
            value = self.callCabal2nix name (./. + "/${path}") { };
          }) hoodlePackages);

        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        fullOverlays = [
          (final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides =
                final.lib.composeExtensions (old.overrides or (_: _: { }))
                haskellOverlay;
            });
          })
        ];

      in {
        # This package set is only useful for CI build test.
        # In practice, users will create a development environment composed by overlays.
        packages = let
          packagesOnGHC = ghcVer:
            let
              overlayGHC = final: prev: {
                haskellPackages = prev.haskell.packages.${ghcVer};
              };

              newPkgs = import nixpkgs {
                overlays = [ overlayGHC ] ++ fullOverlays;
                inherit system;
                config.allowBroken = true;
              };

              individualPackages = builtins.listToAttrs (builtins.map
                ({ name, ... }: {
                  name = ghcVer + "_" + name;
                  value = builtins.getAttr name newPkgs.haskellPackages;
                }) hoodlePackages);

              allEnv = let
                hsenv = newPkgs.haskellPackages.ghcWithPackages (p:
                  let
                    deps =
                      builtins.map ({ name, ... }: p.${name}) hoodlePackages;
                  in deps);
              in newPkgs.buildEnv {
                name = "all-packages";
                paths = [ hsenv ];
              };
            in individualPackages // { "${ghcVer}_all" = allEnv; };

        in packagesOnGHC "ghc8107";
        # // packagesOnGHC "ghc884"
        #  // packagesOnGHC "ghc901"
        # // packagesOnGHC "ghc921"
        # // packagesOnGHC "ghcHEAD";

        overlays = fullOverlays;

        devShells = let
          mkDevShell = ghcVer:
            let
              overlayGHC = final: prev: {
                haskellPackages = prev.haskell.packages.${ghcVer};
              };

              newPkgs = import nixpkgs {
                overlays = [ overlayGHC ] ++ fullOverlays;
                inherit system;
                config.allowBroken = true;
              };

            in newPkgs.haskellPackages.shellFor {
              packages = ps: builtins.map (name: ps.${name}) hoodlePackageNames;
              buildInputs = [
                newPkgs.haskellPackages.cabal-install
                newPkgs.haskellPackages.hlint
              ] ++
                # haskell-language-server on GHC 9.2.1 is broken yet.
                newPkgs.lib.optional (ghcVer != "ghc921")
                [ newPkgs.haskell-language-server ];
              withHoogle = false;
            };
        in {
          "default" = mkDevShell "ghc921";
          "ghc8107" = mkDevShell "ghc8107";
          "ghc921" = mkDevShell "ghc921";
        };
      });
}
