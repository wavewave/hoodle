{
  description = "Hoodle: pen notetaking program";
  inputs = {
    # for ghc944, libdeflate needs to be packaged with pkg-config
    # also, haskellPackages are updated for gtk.
    # TODO: back to NixOS/nixpkgs/master when handled.
    nixpkgs.url = "github:wavewave/nixpkgs/libdeflate-fix-on-peti-master";
    # build failure due to failing linear_base from nixos 22.05 on.
    nixpkgs_21_11.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
      flake = false;
    };
    TypeCompose = {
      url = "github:conal/TypeCompose/master";
      flake = false;
    };
    ghc_nix = {
      url = "github:wavewave/ghc.nix/fix-hash-again";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
      inputs.all-cabal-hashes.follows = "all-cabal-hashes";
    };
  };
  outputs = inputs@{ self, nixpkgs, nixpkgs_21_11, flake-utils, ... }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs_21_11 = import nixpkgs_21_11 { inherit system; };
        haskellLib = pkgs.haskell.lib;

        parseCabalProject = import ./parse-cabal-project.nix;
        hoodlePackages = parseCabalProject ./cabal.project;
        hoodlePackageNames = builtins.map ({ name, ... }: name) hoodlePackages;


        gtkSystemDeps =
          self: [
            self.epoxy.dev
            self.gd
            self.gtk3
            self.libdatrie
            self.libdeflate
            self.librsvg.dev
            self.libthai
            self.pcre
            self.pcre2
            self.xorg.libXdmcp.dev
            self.libxkbcommon.dev
            self.xorg.libXtst
            self.pkgconfig
          ] ++ self.lib.optional self.stdenv.isLinux [ self.libselinux self.libsepol self.util-linux.dev ];

        # gtk
        haskellOverlay = hself: hsuper:
          {
            "TypeCompose" =
              hself.callCabal2nix "TypeCompose" inputs.TypeCompose { };
          } // builtins.listToAttrs (builtins.map ({ name, path }: {
            inherit name;
            value = hself.callCabal2nix name (./. + "/${path}") { };
          }) hoodlePackages);

        conf94 = hself: hsuper: {
          gio = let gio_t = haskellLib.addPkgconfigDepend hsuper.gio pkgs.pcre2;
          in haskellLib.addExtraLibraries gio_t
          [
            pkgs.util-linux.dev
            pkgs.libselinux
            pkgs.libsepol
            pkgs.pcre
          ];
          gtk3 = let
            gtk3_t =
              haskellLib.addPkgconfigDepend (haskellLib.doJailbreak hsuper.gtk3)
              pkgs.pcre2;
          in haskellLib.addExtraLibraries gtk3_t [
            pkgs.util-linux.dev
            pkgs.libselinux
            pkgs.libsepol
            pkgs.pcre
            pkgs.libthai
            pkgs.libdatrie
            pkgs.xorg.libXdmcp.dev
            pkgs.libdeflate
            pkgs.libxkbcommon.dev
            pkgs.epoxy.dev
            pkgs.xorg.libXtst
          ];
          svgcairo = haskellLib.addPkgconfigDepends hsuper.svgcairo [
            pkgs.pcre2
            pkgs.util-linux.dev
            pkgs.libselinux
            pkgs.libsepol
            pkgs.pcre
            pkgs.librsvg.dev
            pkgs.libdeflate
            pkgs.xorg.libXdmcp.dev
          ];
        };

        hpkgsFor = compiler:
          let
            hsoverlay = if compiler == "ghc944" then
              hself: hsuper:
              (haskellOverlay hself hsuper // conf94 hself hsuper)
            else
              haskellOverlay;
          in pkgs.haskell.packages.${compiler}.extend hsoverlay;

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
        supportedCompilers = [ "ghc925" "ghc944" ];
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
            hsenvWebClient = (hpkgsWebClient).ghcWithPackages (p: [ ]);
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

        # ghc.nix shell
        ghcNixShell = inputs.ghc_nix.outputs.devShells.${system}.default.overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ gtkSystemDeps pkgs;
          shellHook = ''
            export PS1="\n[hoodle-ghc.nix:\w]$ \0"
          '';
        });

      in rec {
        # This package set is only useful for CI build test.
        packages.gtk =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkPkgsFor compiler);

        defaultPackage = packages.gtk.${defaultCompiler}.hoodle;

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
          ghc_nix = ghcNixShell;
        };

      });
}
