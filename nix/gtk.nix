{
  ghc-eventlog-socket,
  TypeCompose,
  pkgs,
  haskellLib,
}: rec {
  parseCabalProject = import ./parse-cabal-project.nix;
  hoodlePackages = parseCabalProject ../cabal.project;
  hoodlePackageNames = builtins.map ({name, ...}: name) hoodlePackages;

  haskellOverlay = hself: hsuper:
    {
      "eventlog-socket" =
        hself.callCabal2nix "eventlog-socket" ghc-eventlog-socket {};
      "TypeCompose" =
        hself.callCabal2nix "TypeCompose" TypeCompose {};
    }
    // builtins.listToAttrs (builtins.map ({
        name,
        path,
      }: {
        inherit name;
        value = hself.callCabal2nix name (../. + "/${path}") {};
      })
      hoodlePackages);

  conf94 = hself: hsuper: {
    gio = let
      gio_t = haskellLib.addPkgconfigDepend hsuper.gio pkgs.pcre2;
    in
      haskellLib.addExtraLibraries gio_t
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
    in
      haskellLib.addExtraLibraries gtk3_t
      [
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

  hpkgsFor = compiler: let
    hsoverlay =
      if compiler == "ghc944"
      then hself: hsuper: (haskellOverlay hself hsuper // conf94 hself hsuper)
      else haskellOverlay;
  in
    pkgs.haskell.packages.${compiler}.extend hsoverlay;

  mkPkgsFor = compiler: let
    hpkgs = hpkgsFor compiler;
  in
    pkgs.lib.genAttrs hoodlePackageNames (name: hpkgs.${name});

  mkShellFor = compiler:
    (hpkgsFor compiler).shellFor {
      packages = ps: builtins.map (name: ps.${name}) hoodlePackageNames;
      buildInputs = [
        pkgs.alejandra
        pkgs.gnome.adwaita-icon-theme
        pkgs.pkg-config
        pkgs.haskell.packages.${compiler}.cabal-install
        pkgs.haskell.packages.${compiler}.haskell-language-server
        pkgs.haskell.packages.${compiler}.implicit-hie
      ];
      shellHook = ''
        export XDG_DATA_DIRS=${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_ICON_DIRS:$XDG_DATA_DIRS
        export PS1="\n[hoodle:\w]$ \0"
      '';
    };
  supportedCompilers = ["ghc925" "ghc944"];
  defaultCompiler = "ghc925";
}
