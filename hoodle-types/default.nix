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
    buildDepends = with haskellngPackages; [ aeson lens cereal mtl strict text uuid ];
  }) {}
