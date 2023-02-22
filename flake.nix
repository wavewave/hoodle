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
    hackage-index = {
      type = "file";
      flake = false;
      url = "https://api.github.com/repos/commercialhaskell/all-cabal-hashes/tarball/dc285e8b0968426833746d3f776657c436fb65e0";
    };
    TypeCompose = {
      url = "github:conal/TypeCompose/master";
      flake = false;
    };
    ghc_nix = {
      url = "github:wavewave/ghc.nix/fix-hash-again";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-unstable.follows = "nixpkgs";
      inputs.all-cabal-hashes.follows = "hackage-index";
    };
    ghc-eventlog-socket = {
      url = "github:wavewave/ghc-eventlog-socket/darwin-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs_21_11,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system: let
      pkgs = import nixpkgs {inherit system;};
      pkgs_21_11 = import nixpkgs_21_11 {inherit system;};
      haskellLib = pkgs.haskell.lib;

      # gtk
      gtk = import ./nix/gtk.nix {
        inherit (inputs) ghc-eventlog-socket TypeCompose;
        inherit pkgs haskellLib;
      };

      # web
      web = import ./nix/web.nix {
        pkgs = pkgs_21_11;
        inherit haskellLib;
      };

      # ghc.nix shell
      ghcNixShell = inputs.ghc_nix.outputs.devShells.${system}.default.overrideAttrs (attrs: {
        buildInputs =
          attrs.buildInputs
          ++ [
            pkgs.epoxy.dev
            pkgs.gd
            pkgs.gtk3
            pkgs.libdatrie
            pkgs.libdeflate
            pkgs.librsvg.dev
            pkgs.libthai
            pkgs.pcre
            pkgs.pcre2
            pkgs.xorg.libXdmcp.dev
            pkgs.libxkbcommon.dev
            pkgs.xorg.libXtst
            pkgs.pkgconfig
          ]
          ++ pkgs.lib.optional pkgs.stdenv.isLinux [pkgs.libselinux pkgs.libsepol pkgs.util-linux.dev];

        shellHook = ''
          export PS1="\n[hoodle-ghc.nix:\w]$ \0"
        '';
      });
    in rec {
      # This package set is only useful for CI build test.
      packages.gtk =
        pkgs.lib.genAttrs gtk.supportedCompilers (compiler: gtk.mkPkgsFor compiler);

      defaultPackage = packages.gtk.${gtk.defaultCompiler}.hoodle;

      inherit (gtk) haskellOverlay;

      devShells = let
        gtkShells =
          pkgs.lib.genAttrs gtk.supportedCompilers
          (compiler: gtk.mkShellFor compiler);
        webShells =
          pkgs.lib.genAttrs web.supportedCompilersWeb
          (compiler: web.mkWebShellFor compiler);
      in rec {
        default = gtk.${gtk.defaultCompiler};
        gtk = gtkShells;
        web = webShells;
        ghc_nix = ghcNixShell;
      };
    });
}
