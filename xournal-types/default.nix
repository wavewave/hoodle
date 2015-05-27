{ mkDerivation, base, bytestring, cereal, containers, lens, stdenv
, strict, TypeCompose
}:
mkDerivation {
  pname = "xournal-types";
  version = "0.5.1";
  src = ./.;
  buildDepends = [
    base bytestring cereal containers lens strict TypeCompose
  ];
  description = "Data types for programs for xournal file format";
  license = stdenv.lib.licenses.bsd3;
}
