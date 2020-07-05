{ mkDerivation, attoparsec, base, bytestring, cairo, cmdargs
, containers, directory, directory-tree, filepath, gtk3
, hoodle-parser, hoodle-render, hoodle-types, HTTP, io-streams
, lens, mtl, network-uri, pdf-toolbox-core, pdf-toolbox-document
, process, stdenv, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "hoodle-publish";
  version = "1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring cairo containers directory
    directory-tree filepath gtk3 hoodle-parser hoodle-render
    hoodle-types HTTP io-streams lens mtl network-uri pdf-toolbox-core
    pdf-toolbox-document process transformers unordered-containers uuid
  ];
  executableHaskellDepends = [
    base cmdargs directory directory-tree filepath gtk3
  ];
  homepage = "http://ianwookim.org/hoodle";
  description = "publish hoodle files as a static web site";
  license = stdenv.lib.licenses.bsd3;
}
