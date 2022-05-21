{
  description = "Hoodle: pen notetaking program";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    poppler = {
      url = "github:wavewave/poppler/0a9ea94415c80595f8ebd7bcb5069493da447996";
      flake = false;
    };
    pdf-toolbox = {
      url =
        "github:wavewave/pdf-toolbox/df9182d2dabc7cfe90b13e43b6a43332f852f23f";
      flake = false;
    };
    gtk2hs = {
      url =
        "/home/wavewave/repo/src/gtk2hs"; # "github:wavewave/gtk2hs/pkgconfig-modversion";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, poppler, pdf-toolbox, gtk2hs }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;
        overlay_deps = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                "gtk2hs-buildtools" =
                  self.callCabal2nix "gtk2hs-buildtools" (gtk2hs + "/tools")
                  { };
                "pdf-toolbox" =
                  self.callCabal2nix "pdf-toolbox" pdf-toolbox { };
                "poppler" = haskellLib.overrideCabal
                  (self.callCabal2nix "poppler" poppler { }) {
                    configureFlags = [
                      "-fgtk3"
                      # NOTE: multiple definition of (libc functions)
                      # disable until find a fix
                      "--disable-shared"
                    ];
                    hardeningDisable = [ "fortify" ];
                    #librarySystemDepends = [ final.poppler_gi ];
                    setupHaskellDepends =
                      [ self.base self.Cabal self.gtk2hs-buildtools ];
                    libraryPkgconfigDepends =
                      [ final.gtk3 final.pango final.poppler_gi ];
                  };

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
              mypoppler = newPkgs.haskellPackages.poppler;

            in individualPackages // {
              "${ghcVer}_all" = allEnv;
              "${ghcVer}_poppler" = mypoppler;
            };

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
              packages = ps:
                [
                  # ps.happy
                ]; # ps: builtins.map (name: ps.${name}) hoodlePackageNames;
              buildInputs = [
                newPkgs.gtk3
                newPkgs.poppler_gi
                newPkgs.pkg-config
                newPkgs.haskellPackages.cabal-install
                newPkgs.haskellPackages.happy
                newPkgs.haskellPackages.hlint
              ] ++
                # haskell-language-server on GHC 9.2.1 is broken yet.
                newPkgs.lib.optional (ghcVer != "ghc921")
                [ newPkgs.haskell-language-server ];
              withHoogle = false;
            };
        in {
          "default" = mkDevShell "ghc8107";
          "ghc8107" = mkDevShell "ghc8107";
          "ghc921" = mkDevShell "ghc921";
        };
      });
}
