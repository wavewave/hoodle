{ mkDerivation, base, containers, mtl, stdenv, strict }:
mkDerivation {
  pname = "hoodle-util";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl strict ];
  description = "Utility functions for hoodle";
  license = stdenv.lib.licenses.bsd3;
}
