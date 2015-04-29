#{ nixpkgs ? import <nixpkgs> {}, 
#  haskellngPackages ? nixpkgs.haskellngPackages }:

#haskellngPackages ? (import <nixpkgs> {}).haskellngPackages }:


let nixpkgs = import <nixpkgs> {}; 
    haskellngPackages = nixpkgs.haskellngPackages;
    stdenv = nixpkgs.stdenv;
in 

haskellngPackages.callPackage ({mkDerivation}:
  mkDerivation {  
    pname = "hoodle-types";
    version = "0.3.999";
    src = ./.;
    license = stdenv.lib.licenses.bsd3;
    buildDepends = with haskellngPackages; [ lens cereal mtl strict text uuid ];
  }) {}
