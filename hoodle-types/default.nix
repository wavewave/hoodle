{ mkDerivation, aeson, base, bytestring, cereal, containers, lens
, mtl, stdenv, strict, text, uuid, vector
}:
mkDerivation {
  pname = "hoodle-types";
  version = "0.3.999";
  src = ./.;
  buildDepends = [
    aeson base bytestring cereal containers lens mtl strict text uuid
    vector
  ];
  description = "Data types for programs for hoodle file format";
  license = stdenv.lib.licenses.bsd3;
}
