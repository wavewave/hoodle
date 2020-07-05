{ mkDerivation, aeson, aeson-pretty, array, attoparsec, base
, base64-bytestring, binary, bytestring, cairo, cereal
, configurator, containers, coroutine-object, Diff, directory
, errors, filepath, fsnotify, gd, gtk3, hoodle-builder
, hoodle-parser, hoodle-publish, hoodle-render, hoodle-types
, hoodle-util, http-types, lens, libX11, libXi, monad-loops, mtl
, network, network-info, network-simple, network-uri, pango
, poppler, process, pureMD5, resourcet, stdenv, stm, strict
, svgcairo, system-filepath, template-haskell, text, time
, transformers, transformers-free, unordered-containers, uuid
, vector, xournal-parser
}:
mkDerivation {
  pname = "hoodle-core";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty array attoparsec base base64-bytestring binary
    bytestring cairo cereal configurator containers coroutine-object
    Diff directory errors filepath fsnotify gd gtk3 hoodle-builder
    hoodle-parser hoodle-publish hoodle-render hoodle-types hoodle-util
    http-types lens monad-loops mtl network network-info network-simple
    network-uri pango poppler process pureMD5 resourcet stm strict
    svgcairo system-filepath template-haskell text time transformers
    transformers-free unordered-containers uuid vector xournal-parser
  ];
  librarySystemDepends = [ libX11 libXi ];
  homepage = "http://ianwookim.org/hoodle";
  description = "Core library for hoodle";
  license = stdenv.lib.licenses.bsd3;
}
