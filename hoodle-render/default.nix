{ mkDerivation, base, base64-bytestring, bytestring, cairo
, containers, directory, filepath, gd, gtk, hashable, hoodle-types
, lens, monad-loops, mtl, poppler, stdenv, stm, strict, svgcairo
, time, transformers, unix, unordered-containers, uuid
}:
mkDerivation {
  pname = "hoodle-render";
  version = "0.5.0";
  src = ./.;
  buildDepends = [
    base base64-bytestring bytestring cairo containers directory
    filepath gd gtk hashable hoodle-types lens monad-loops mtl poppler
    stm strict svgcairo time transformers unix unordered-containers
    uuid
  ];
  description = "Hoodle file renderer";
  license = stdenv.lib.licenses.bsd3;
}
