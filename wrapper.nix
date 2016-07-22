{ stdenv, haskellPackages, gtk3, poppler, makeWrapper, packages }:

let hsconfig = import ./default.nix { poppler = poppler; gtk3 = gtk3; };
    newHaskellPackages = haskellPackages.override { overrides = hsconfig; };
    hoodleEnv = newHaskellPackages.ghcWithPackages (p: with p; [ hoodle ] ++ packages p);
in stdenv.mkDerivation {
     name = "hoodle-with-packages";

     nativeBuildInputs = [ makeWrapper ];

     buildCommand = ''
       mkdir -p $out/bin
       makeWrapper ${hoodleEnv}/bin/hoodle $out/bin/hoodle \
        --set NIX_GHC "${hoodleEnv}/bin/ghc" 
     '';
   }

#name = "testenv";
#     #propagatedBuildInputs = [ pkgs.wrapGAppsHook ] ;
#     buildInputs = [ hsenv ];
#   }

#     hoodleEnv = ghcWithPackages (self: [ self.hoodle ] ++ packages self);
# in stdenv.mkDerivation {
#   name = "hoodle-with-packages";

#   nativeBuildInputs = [ makeWrapper ];

#   buildCommand = ''
#     mkdir -p $out/bin
#     makeWrapper ${hoodleEnv}/bin/hoodle $out/bin/hoodle \
#       --set NIX_GHC "${hoodleEnv}/bin/ghc" \
#   '';

#   # trivial derivation
#   preferLocalBuild = true;
#   allowSubstitutes = false;
# }
