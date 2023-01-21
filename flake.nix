{
  description = "Hoodle: pen notetaking program";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    # build failure due to failing linear_base from nixos 22.05 on.
    nixpkgs_21_11.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    TypeCompose = {
      url = "github:conal/TypeCompose/master";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, nixpkgs_21_11, flake-utils, ... }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs_21_11 = import nixpkgs_21_11 {
          inherit system;
          #config.allowBroken = true;
        };
        haskellLib = pkgs.haskell.lib;

        parseCabalProject = import ./parse-cabal-project.nix;
        hoodlePackages = parseCabalProject ./cabal.project;
        hoodlePackageNames = builtins.map ({ name, ... }: name) hoodlePackages;

        # gtk
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
              export PS1="\n[hoodle:\w]$ \0"
            '';
          };
        supportedCompilers = [ "ghc925" ];
        defaultCompiler = "ghc925";

        # web
        haskellOverlayWebClient = hself: hsuper: {
          coroutine-object =
            hself.callCabal2nix "coroutine-object" ./coroutine-object { };
          hoodle-util = hself.callCabal2nix "hoodle-util" ./util { };
          comonad = haskellLib.dontCheck hsuper.comonad;
          semigroupoids = haskellLib.dontCheck hsuper.semigroupoids;
          QuickCheck = haskellLib.dontCheck hsuper.QuickCheck;
          scientific = haskellLib.dontCheck hsuper.scientific;
          tasty-quickcheck = haskellLib.dontCheck hsuper.tasty-quickcheck;
          time-compat = haskellLib.dontCheck hsuper.time-compat;
        };
        hpkgsWebClient =
          pkgs_21_11.haskell.packages.ghcjs.extend haskellOverlayWebClient;

        mkWebShellFor = compiler:
          let
            hsenvWebServer =
              pkgs_21_11.haskell.packages.${compiler}.ghcWithPackages (p: [
                p.acid-state
                p.microlens
                p.microlens-th
                p.monad-loops
                p.servant
                p.servant-server
                p.websockets
              ]);
            hsenvWebClient = (hpkgsWebClient).ghcWithPackages (p:
              [
                #p.coroutine-object
                #p.ghcjs-base
                #p.ghcjs-dom
                ##p.hoodle-util
                #p.microlens
                #p.microlens-th
              ]);
          in pkgs_21_11.mkShell {
            name = "hoodle-web-shell";
            buildInputs = [
              pkgs_21_11.nodePackages.http-server
              hsenvWebServer
              hsenvWebClient
              pkgs_21_11.cabal-install
              pkgs_21_11.ormolu
            ];
          };

        supportedCompilersWeb = [ "ghc8107" ];
        defaultCompilerWeb = "ghc8107";

      in rec {
        # This package set is only useful for CI build test.
        packages =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkPkgsFor compiler);

        defaultPackage = packages.${defaultCompiler}.hoodle;

        inherit haskellOverlay;

        devShells = let
          gtkShells = pkgs.lib.genAttrs supportedCompilers
            (compiler: mkShellFor compiler);
          webShells = pkgs.lib.genAttrs supportedCompilersWeb
            (compiler: mkWebShellFor compiler);
        in rec {
          default = gtk.${defaultCompiler};
          gtk = gtkShells;
          web = webShells;
        };

      });
}
