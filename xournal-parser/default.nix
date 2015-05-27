{ mkDerivation, attoparsec, attoparsec-conduit, base, bytestring
, conduit, conduit-extra, containers, exceptions, lens, mtl, stdenv
, strict, text, transformers, xml-conduit, xml-types, xournal-types
, zlib-conduit
}:
mkDerivation {
  pname = "xournal-parser";
  version = "0.5.1";
  src = ./.;
  buildDepends = [
    attoparsec attoparsec-conduit base bytestring conduit conduit-extra
    containers exceptions lens mtl strict text transformers xml-conduit
    xml-types xournal-types zlib-conduit
  ];
  homepage = "http://ianwookim.org/hoodle";
  description = "Xournal file parser";
  license = stdenv.lib.licenses.bsd3;
}
