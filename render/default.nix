{ mkDerivation, base, base64-bytestring, bytestring, cairo
, containers, directory, filepath, gd, gtk3, hashable, hoodle-types
, hoodle-util, lens, monad-loops, mtl, poppler, stdenv, stm, strict
, svgcairo, time, transformers, unix, unordered-containers, uuid
}:
mkDerivation {
  pname = "hoodle-render";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring cairo containers directory
    filepath gd gtk3 hashable hoodle-types hoodle-util lens monad-loops
    mtl poppler stm strict svgcairo time transformers unix
    unordered-containers uuid
  ];
  description = "Hoodle file renderer";
  license = stdenv.lib.licenses.bsd3;
}
