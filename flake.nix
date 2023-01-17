{
  description = "Hoodle: pen notetaking program";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    TypeCompose = {
      url = "github:conal/TypeCompose/master";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellLib = pkgs.haskell.lib;

        parseCabalProject = import ./parse-cabal-project.nix;
        hoodlePackages = parseCabalProject ./cabal.project;
        hoodlePackageNames = builtins.map ({ name, ... }: name) hoodlePackages;
        haskellOverlay = hself: hsuper:
          {
            "TypeCompose" =
              hself.callCabal2nix "TypeCompose" inputs.TypeCompose { };
          } // builtins.listToAttrs (builtins.map ({ name, path }: {
            inherit name;
            value = hself.callCabal2nix name (./. + "/${path}") { };
          }) hoodlePackages);

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend haskellOverlay;

        mkPkgsFor = compiler:
          let hpkgs = hpkgsFor compiler;
          in pkgs.lib.genAttrs hoodlePackageNames (name: hpkgs.${name});

        mkShellFor = compiler:
          (hpkgsFor compiler).shellFor {
            packages = ps: builtins.map (name: ps.${name}) hoodlePackageNames;
            buildInputs = [
              pkgs.gnome.adwaita-icon-theme
              pkgs.pkg-config
              pkgs.haskell.packages.${compiler}.cabal-install
            ];
            shellHook = ''
              export XDG_DATA_DIRS=${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_ICON_DIRS:$XDG_DATA_DIRS
            '';
          };

        supportedCompilers = [ "ghc925" ];
        defaultCompiler = "ghc925";

      in rec {
        # This package set is only useful for CI build test.
        packages =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkPkgsFor compiler);

        defaultPackage = packages.${defaultCompiler}.hoodle;

        inherit haskellOverlay;

        devShells = let
          shells = pkgs.lib.genAttrs supportedCompilers
            (compiler: mkShellFor compiler);
        in { default = shells.${defaultCompiler}; } // shells;

      });
}
