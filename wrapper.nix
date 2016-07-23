{ stdenv, haskellPackages, gtk3, poppler, makeWrapper, wrapGAppsHook
, gnome3, packages }:

let hsconfig = import ./default.nix { poppler = poppler; gtk3 = gtk3; };
    newHaskellPackages = haskellPackages.override { overrides = hsconfig; };
    hoodleEnv = newHaskellPackages.ghcWithPackages (p: with p; [ hoodle ] ++ packages p);
in stdenv.mkDerivation {
     name = "hoodle-with-packages";

     nativeBuildInputs = [ makeWrapper ];
     propagatedBuildInputs = [ wrapGAppsHook];
     buildInputs = [ gnome3.defaultIconTheme
                     gnome3.gsettings_desktop_schemas
                   ] ;


     buildCommand = ''
       mkdir -p $out/bin
       makeWrapper ${hoodleEnv}/bin/hoodle $out/bin/hoodle \
        --set NIX_GHC "${hoodleEnv}/bin/ghc" \
        --set XDG_DATA_DIRS "${gtk3}/share/gsettings-schemas/${gtk3.name}:${gnome3.gsettings_desktop_schemas}/share/gsettings-schemas/${gnome3.gsettings_desktop_schemas.name}"
     '';
   }

# --set XDG_DATA_DIRS "${gtk3}/share/gsettings-schemas/${gtk3.name}/glib-2.0/schemas:${gnome3.gsettings_desktop_schemas}/share/gsettings-schemas/${gnome3.gsettings_desktop_schemas.name}