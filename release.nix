{ pkgs ? import <nixpkgs> {} }:

let fix = f: let x = f x // { __unfix__ = f; }; in x;

    ghc = pkgs.haskell-ng.packages.ghc7101;
    ghcjs = pkgs.haskell-ng.packages.ghcjs;
    stdenv = pkgs.stdenv;
    
    hoodlepkgs = import ./default.nix { inherit stdenv ghc; };
in fix hoodlepkgs

#     #pkgs = import <nixpkgs> {};
#     haskellngPackages = pkgs.haskellngPackages;
#     stdenv = pkgs.stdenv;
#     jobs = rec {
#       popplerNew =
#         haskellngPackages.callPackage ({mkDerivation}:
# 	  mkDerivation {
#             pname = "poppler";
# 	    version = "0.13.1";
# 	    src = pkgs.fetchgit { url = "https://github.com/wavewave/poppler.git";
# 				  rev = "05123d9f054a353ea0f06a3179f3f2c773594079";
# 				  sha256 = "6e142219c94c9375f54c55825ab952fa0c1d4358b2706fad63f18885ebf77a40"; 
# 				};
# 	    buildDepends = with haskellngPackages; [
# 	      array base bytestring cairo containers glib gtk mtl
# 	    ];
# 	    buildTools = with haskellngPackages; [ gtk2hs-buildtools ];
# 	    pkgconfigDepends = [ pkgs.gtk pkgs.glib pkgs.gdk_pixbuf pkgs.pango pkgs.poppler ];
# 	    homepage = "http://projects.haskell.org/gtk2hs";
# 	    description = "Binding to the Poppler";
# 	    license = stdenv.lib.licenses.gpl2;			      
# 	  }) {};
#       coroutine-object = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "coroutine-object";
#             version = "0.3";
#             src = ./coroutine-object;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ cereal containers either lens mtl safecopy transformers transformers-free uuid ];
#         }) {};

#       hoodle-types = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "hoodle-types";
#             version = "0.3.999";
#             src = ./hoodle-types;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ aeson lens cereal mtl strict text uuid ];
#         }) {};
#       hoodle-parser = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "hoodle-parser";
#             version = "0.3.999";
#             src = ./hoodle-parser;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ attoparsec either directory hoodle-types lens cereal mtl strict text xournal-types ];
#         }) {};
#       xournal-types = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "xournal-types";
#             version = "0.5.1";
#             src = ./xournal-types;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ cereal lens strict TypeCompose ];
#         }) {};
#       xournal-parser = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "xournal-parser";
#             version = "0.5.1";
#             src = ./xournal-parser;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ attoparsec attoparsec-conduit cereal conduit conduit-extra containers either
# 	                                             exceptions directory lens mtl strict text transformers xml-conduit xml-types
# 						     xournal-types
# 						     zlib-conduit ];
#         }) {};	
#       hoodle-builder = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "hoodle-builder";
#             version = "0.3.999";
#             src = ./hoodle-builder;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ blaze-builder double-conversion hoodle-types lens strict text ];
#         }) {};
#       hoodle-render = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "hoodle-render";
#             version = "0.5.0";
#             src = ./hoodle-render;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ base64-bytestring cairo containers directory filepath gd hashable hoodle-types 
#                                                      lens monad-loops mtl popplerNew stm strict svgcairo time transformers unix unordered-containers 
#                                                      uuid ];
#         }) {};
#       hoodle-publish = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "hoodle-publish";
#             version = "0.2.0";
#             src = ./hoodle-publish;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ attoparsec cairo cmdargs containers directory directory-tree filepath 
#                                                      gtk hoodle-parser hoodle-render hoodle-types 
#                                                      HTTP io-streams lens mtl network-uri pdf-toolbox-core pdf-toolbox-document 
#                                                      process transformers unordered-containers uuid ];
#         }) {};
#       hoodle-core = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "hoodle-core";
#             version = "0.15.0";
#             src = ./hoodle-core;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ aeson aeson-pretty array attoparsec base64-bytestring binary cairo 
#                                                      case-insensitive cereal containers configurator coroutine-object Diff 
#                                                      directory either errors filepath fsnotify gd handa-gdata 
#                                                      hoodle-builder hoodle-parser hoodle-publish hoodle-render hoodle-types
#                                                      http-types lens monad-loops mtl network-uri old-locale pango popplerNew process 
#                                                      pureMD5 stm strict svgcairo system-filepath template-haskell text time 
#                                                      transformers transformers-free unordered-containers uuid vector websockets 
#                                                      xournal-parser ];
#         }) {};
#       hoodle = 
#         haskellngPackages.callPackage ({mkDerivation}: 
#           mkDerivation {
#             pname = "hoodle";
#             version = "0.4.0";
#             src = ./hoodle;
#             license = stdenv.lib.licenses.bsd3;
#             buildDepends = with haskellngPackages; [ cmdargs configurator containers directory dyre filepath  hoodle-core mtl ];
#         }) {};
#     };
# in jobs