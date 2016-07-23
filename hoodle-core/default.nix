{ mkDerivation, aeson, aeson-pretty, array, attoparsec, base
, base64-bytestring, binary, bytestring, cairo, case-insensitive
, cereal, configurator, containers, coroutine-object, Diff
, directory, either, errors, filepath, fsnotify, gd, gtk3
, handa-gdata, hoodle-builder, hoodle-parser, hoodle-publish
, hoodle-render, hoodle-types, http-types, lens, libX11, libXi
, monad-loops, mtl, network-uri, pango, poppler, process, pureMD5
, stdenv, stm, strict, svgcairo, system-filepath, template-haskell
, text, time, transformers, transformers-free, unordered-containers
, uuid, vector, websockets, xournal-parser, dyre, pkgconfig
}:
mkDerivation {
  pname = "hoodle-core";
  version = "0.16";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty array attoparsec base base64-bytestring binary
    bytestring cairo case-insensitive cereal configurator containers
    coroutine-object Diff directory either errors filepath fsnotify gd
    gtk3 handa-gdata hoodle-builder hoodle-parser hoodle-publish
    hoodle-render hoodle-types http-types lens monad-loops mtl
    network-uri pango poppler process pureMD5 stm strict svgcairo
    system-filepath template-haskell text time transformers
    transformers-free unordered-containers uuid vector websockets
    xournal-parser dyre
  ];
  configureFlags = [ "-fdyre" ];
  librarySystemDepends = [ libX11 libXi ];
  buildDepends = [ pkgconfig ];
  homepage = "http://ianwookim.org/hoodle";
  description = "Core library for hoodle";
  license = stdenv.lib.licenses.bsd3;
}
