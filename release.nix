#{ hoodlesrc ? { outPath = ./.; revCount = 1234; shortRev = "abcdef"; }
{ pkgs ? import <nixpkgs> {} }:

let
    #pkgs = import <nixpkgs> {};
    haskellngPackages = pkgs.haskellngPackages;
    stdenv = pkgs.stdenv;
    jobs = rec { 
      coroutine-object = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "coroutine-object";
            version = "0.3";
            src = ./coroutine-object;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ cereal containers either lens mtl safecopy transformers transformers-free uuid ];
        }) {};

      hoodle-types = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle-types";
            version = "0.3.999";
            src = ./hoodle-types;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ aeson lens cereal mtl strict text uuid ];
        }) {};
      hoodle-parser = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle-parser";
            version = "0.3.999";
            src = ./hoodle-parser;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ attoparsec either directory hoodle-types lens cereal mtl strict text xournal-types ];
        }) {};
      xournal-types = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "xournal-types";
            version = "0.5.1";
            src = ./xournal-types;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ cereal lens strict TypeCompose ];
        }) {};
      hoodle-builder = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle-builder";
            version = "0.3.999";
            src = ./hoodle-builder;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ blaze-builder double-conversion hoodle-types lens strict text ];
        }) {};
      hoodle-render = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle-render";
            version = "0.5.0";
            src = ./hoodle-render;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ base64-bytestring cairo containers directory filepath gd hashable hoodle-types 
                                                     lens monad-loops mtl poppler stm strict svgcairo time transformers unix unordered-containers 
                                                     uuid ];
        }) {};
      hoodle-publish = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle-publish";
            version = "0.2.0";
            src = ./hoodle-publish;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ attoparsec cairo cmdargs containers directory directory-tree filepath 
                                                     gtk hoodle-parser hoodle-render hoodle-types 
                                                     HTTP io-streams lens mtl network-uri pdf-toolbox-core pdf-toolbox-document 
                                                     process transformers unordered-containers uuid ];
        }) {};
      hoodle-core = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle-core";
            version = "0.15.0";
            src = ./hoodle-core;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ aeson aeson-pretty array attoparsec base64-bytestring binary cairo 
                                                     case-insensitive cereal containers configurator coroutine-object Diff 
                                                     directory either errors filepath fsnotify gd handa-gdata 
                                                     hoodle-builder hoodle-parser hoodle-publish hoodle-render hoodle-types
                                                     http-types lens monad-loops mtl network-uri old-locale pango poppler process 
                                                     pureMD5 stm strict svgcairo system-filepath template-haskell text time 
                                                     transformers transformers-free unordered-containers uuid vector websockets 
                                                     xournal-parser ];
        }) {};
      hoodle = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle";
            version = "0.4.0";
            src = ./hoodle;
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ cmdargs configurator containers directory dyre filepath  hoodle-core mtl ];
        }) {};
    };
in jobs