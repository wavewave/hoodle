{ hoodlesrc ? { outPath = ./.; revCount = 1234; shortRev = "abcdef"; } }:

let pkgs = import <nixpkgs> {};
    haskellngPackages = pkgs.haskellngPackages;
    stdenv = pkgs.stdenv;
    jobs = rec { 
      hoodle-types = 
        haskellngPackages.callPackage ({mkDerivation}: 
          mkDerivation {
            pname = "hoodle-types";
            version = "0.3.999";
            src = hoodlesrc.outPath + "/hoodle-types";
            license = stdenv.lib.licenses.bsd3;
            buildDepends = with haskellngPackages; [ lens cereal mtl strict text uuid ];
        }) {};
    };
in jobs