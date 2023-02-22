{
  ghc_nix,
  system,
  pkgs,
}: {
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
      ++ pkgs.lib.optional pkgs.stdenv.isLinux [pkgs.libselinux pkgs.libsepol pkgs.util-linux.dev];

    shellHook = ''
      export PS1="\n[hoodle-ghc.nix:\w]$ \0"
    '';
  });
}
