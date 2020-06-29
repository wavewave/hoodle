{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "hoodle-util";
  version = "0.6";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Utility functions for hoodle";
  license = stdenv.lib.licenses.bsd3;
}
