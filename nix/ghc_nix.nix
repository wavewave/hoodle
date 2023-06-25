{
  ghc_nix,
  system,
  pkgs,
}: let
   fontconf = pkgs.makeFontsConf { fontDirectories = [pkgs.freefont_ttf]; };
in {
  # ghc.nix shell
  ghcNixShell = ghc_nix.outputs.devShells.${system}.default.overrideAttrs (attrs: {
    buildInputs =
      attrs.buildInputs
      ++ [
        pkgs.epoxy.dev
        pkgs.gd
        pkgs.gobject-introspection
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
      ++ pkgs.lib.optional pkgs.stdenv.isLinux [pkgs.libselinux.dev pkgs.libsepol.dev pkgs.util-linux.dev];

    shellHook = ''
      export FONTCONFIG_FILE="${fontconf}"
      export PANGOCAIRO_BACKEND=fc
      export PS1="\n[hoodle-ghc.nix:\w]$ \0"
    '';
  });
}
