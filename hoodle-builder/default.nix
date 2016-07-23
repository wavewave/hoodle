{ mkDerivation, base, blaze-builder, bytestring, double-conversion
, hoodle-types, lens, stdenv, strict, text
}:
mkDerivation {
  pname = "hoodle-builder";
  version = "0.4";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-builder bytestring double-conversion hoodle-types lens
    strict text
  ];
  description = "text builder for hoodle file format";
  license = stdenv.lib.licenses.bsd3;
}
