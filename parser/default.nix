{ mkDerivation, attoparsec, base, bytestring, containers, directory
, either, errors, hoodle-types, lens, mtl, stdenv, strict, text
, transformers, xournal-types
}:
mkDerivation {
  pname = "hoodle-parser";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory either errors hoodle-types
    lens mtl strict text transformers xournal-types
  ];
  homepage = "http://ianwookim.org/hoodle";
  description = "Hoodle file parser";
  license = stdenv.lib.licenses.bsd3;
}
