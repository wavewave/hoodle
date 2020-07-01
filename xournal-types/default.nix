{ mkDerivation, base, bytestring, cereal, containers, microlens, stdenv
, strict, TypeCompose
}:
mkDerivation {
  pname = "xournal-types";
  version = "0.5.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal containers microlens strict TypeCompose
  ];
  description = "Data types for programs for xournal file format";
  license = stdenv.lib.licenses.bsd3;
}
