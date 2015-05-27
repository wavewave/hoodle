{ mkDerivation, base, bytestring, cereal, containers, either, lens
, mtl, safecopy, stdenv, transformers, transformers-free, uuid
}:
mkDerivation {
  pname = "coroutine-object";
  version = "0.3";
  src = ./.;
  buildDepends = [
    base bytestring cereal containers either lens mtl safecopy
    transformers transformers-free uuid
  ];
  description = "Object-oriented programming realization using coroutine";
  license = stdenv.lib.licenses.bsd3;
}
