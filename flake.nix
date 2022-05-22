{
  description = "Hoodle: pen notetaking program";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    poppler = {
      url = "github:wavewave/poppler/4e7fc8364f89a4757f67fa9715471b5e97cd86f0";
      flake = false;
    };
    pdf-toolbox = {
      url = "github:wavewave/pdf-toolbox/for-hoodle";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, poppler, pdf-toolbox }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;
        overlay_deps = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                "pdf-toolbox-content" = self.callCabal2nix "pdf-toolbox-content"
                  (pdf-toolbox + "/content") { };
                "pdf-toolbox-core" =
                  self.callCabal2nix "pdf-toolbox-core" (pdf-toolbox + "/core")
                  { };
                "pdf-toolbox-document" =
                  self.callCabal2nix "pdf-toolbox-document"
                  (pdf-toolbox + "/document") { };
                "poppler" = let
                  p = self.callCabal2nix "poppler" poppler { };
                  p1 = haskellLib.appendConfigureFlags p [ "-fgtk3" ];
                  p2 = haskellLib.overrideCabal p1 (drv: {
                    libraryPkgconfigDepends = [ final.gtk3 final.poppler_0_61 ];
                  });
                in haskellLib.disableHardening p2 [ "fortify" ];
              });
          });
        };

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
          overlay_deps
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

            in individualPackages // {
              "${ghcVer}_all" = allEnv;
              "${ghcVer}_poppler" = newPkgs.haskellPackages.poppler;
              "${ghcVer}_pango" = newPkgs.haskellPackages.pango;
              "${ghcVer}_gi-poppler" = newPkgs.haskellPackages.gi-poppler;
              "${ghcVer}_pdf-toolbox-content" =
                newPkgs.haskellPackages.pdf-toolbox-content;
              "${ghcVer}_pdf-toolbox-core" =
                newPkgs.haskellPackages.pdf-toolbox-core;
              "${ghcVer}_pdf-toolbox-document" =
                newPkgs.haskellPackages.pdf-toolbox-document;
            };

        in packagesOnGHC "ghc901" // packagesOnGHC "ghc884";

        # NOTE: GHC 8.10.7 has a problem with poppler (multiple definition of libc functions)
        # gi-poppler is buildable on nixpkgs without custom overlay up to GHC 9.0.1

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

              hsenv = newPkgs.haskellPackages.ghcWithPackages
                (p: [ p.gtk3 p.gtk2hs-buildtools p.pango ]);

            in newPkgs.haskellPackages.shellFor {
              packages = ps: builtins.map (name: ps.${name}) hoodlePackageNames;
              buildInputs = [
                hsenv
                newPkgs.pkg-config
                newPkgs.haskellPackages.cabal-install
                newPkgs.haskellPackages.happy
                newPkgs.haskellPackages.hlint
              ] ++
                # haskell-language-server on GHC 9.2.1 is broken yet.
                newPkgs.lib.optional (ghcVer != "ghc921")
                [ newPkgs.haskell-language-server ];
              #withHoogle = false;
            };
        in {
          "default" = mkDevShell "ghc884";
          "ghc884" = mkDevShell "ghc884";
          "ghc901" = mkDevShell "ghc901";
        };
      });
}
