{ mkDerivation, aeson, base, bytestring, cereal, containers, microlens
, mtl, stdenv, strict, text, uuid, vector
}:
mkDerivation {
  pname = "hoodle-types";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cereal containers microlens mtl strict text uuid
    vector
  ];
  description = "Data types for programs for hoodle file format";
  license = stdenv.lib.licenses.bsd3;
}
