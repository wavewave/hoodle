{
  description = "Hoodle: pen notetaking program";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    #poppler = {
    #  url = "github:wavewave/poppler/4e7fc8364f89a4757f67fa9715471b5e97cd86f0";
    #  flake = false;
    #};
    #pdf-toolbox = {
    #  url = "github:wavewave/pdf-toolbox/for-hoodle";
    #  flake = false;
    #};
    TypeCompose = {
      url = "github:conal/TypeCompose/master";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;
        overlay_deps = final: prev: {
          #poppler_0_61 =
          #  final.callPackage ./nix/poppler_0_61.nix { lcms = final.lcms2; };
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                "TypeCompose" =
                  self.callCabal2nix "TypeCompose" inputs.TypeCompose { };
                #"pdf-toolbox-content" = self.callCabal2nix "pdf-toolbox-content"
                #  (inputs.pdf-toolbox + "/content") { };
                #"pdf-toolbox-core" = self.callCabal2nix "pdf-toolbox-core"
                #  (inputs.pdf-toolbox + "/core") { };
                #"pdf-toolbox-document" =
                #  self.callCabal2nix "pdf-toolbox-document"
                #  (inputs.pdf-toolbox + "/document") { };
                #"poppler" = let
                #  p = self.callCabal2nix "poppler" inputs.poppler { };
                #  p1 = haskellLib.appendConfigureFlags p [ "-fgtk3" ];
                #  p2 = haskellLib.overrideCabal p1 (drv: {
                #    libraryPkgconfigDepends = [ final.gtk3 final.poppler_0_61 ];
                #  });
                #in haskellLib.disableHardening p2 [ "fortify" ];
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

      in rec {
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

          # NOTE: GHC 8.10.7 has a problem with poppler (multiple definition of libc functions)
          # gi-poppler is buildable on nixpkgs without custom overlay up to GHC 9.0.1
        in packagesOnGHC "ghc925";

        defaultPackage = packages.ghc925_all;

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
                newPkgs.gnome.adwaita-icon-theme
                newPkgs.pkg-config
                newPkgs.haskell.packages.ghc8107.cabal-install
              ];
              shellHook = ''
                export XDG_DATA_DIRS=${newPkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${newPkgs.gsettings-desktop-schemas.name}:${newPkgs.gtk3}/share/gsettings-schemas/${newPkgs.gtk3.name}:$XDG_ICON_DIRS:$XDG_DATA_DIRS
              '';
            };
        in rec {
          "default" = ghc925;
          "ghc925" = mkDevShell "ghc925";
          "ghc944" = mkDevShell "ghc944";
        };
      });
}
