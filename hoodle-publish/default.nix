{ mkDerivation, attoparsec, base, bytestring, cairo, cmdargs
, containers, directory, directory-tree, filepath, gtk
, hoodle-parser, hoodle-render, hoodle-types, HTTP, io-streams
, lens, mtl, network-uri, pdf-toolbox-core, pdf-toolbox-document
, process, stdenv, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "hoodle-publish";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec base bytestring cairo cmdargs containers directory
    directory-tree filepath gtk hoodle-parser hoodle-render
    hoodle-types HTTP io-streams lens mtl network-uri pdf-toolbox-core
    pdf-toolbox-document process transformers unordered-containers uuid
  ];
  homepage = "http://ianwookim.org/hoodle";
  description = "publish hoodle files as a static web site";
  license = stdenv.lib.licenses.bsd3;
}
