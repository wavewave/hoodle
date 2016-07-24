{ stdenv, haskellPackages, gtk3, poppler, makeWrapper, wrapGAppsHook
, gnome3, packages, pkgconfig }:

let hsconfig = import ./default.nix { poppler = poppler; gtk3 = gtk3; };
    newHaskellPackages = haskellPackages.override { overrides = hsconfig; };
    hoodleEnv = newHaskellPackages.ghcWithPackages (p: with p; [ hoodle ] ++ packages p);
    #(p: with p; [
    #hoodle-parser hoodle-publish hoodle-render coroutine-object 
#Diff aeson-pretty case-insensitive configurator fsnotify handa-gdata hoodle-builder http-types pureMD5 system-filepath transformers-free websockets xournal-parser


in stdenv.mkDerivation {
     name = "hoodle-with-packages";

     nativeBuildInputs = [ makeWrapper ];
     #buildInputs = [ hoodleEnv pkgconfig gtk3 ];

     buildCommand = ''
       mkdir -p $out/bin
       makeWrapper ${hoodleEnv}/bin/hoodle $out/bin/hoodle \
        --set NIX_GHC "${hoodleEnv}/bin/ghc" \
        --set XDG_DATA_DIRS "${gtk3.out}/share/gsettings-schemas/${gtk3.name}:${gnome3.gsettings_desktop_schemas}/share/gsettings-schemas/${gnome3.gsettings_desktop_schemas.name}:${gnome3.defaultIconTheme}/share"
     '';
   }
