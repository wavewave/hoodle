{ mkDerivation, base, either, free, mtl, stdenv, transformers }:
mkDerivation {
  pname = "coroutine-object";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [ base either free mtl transformers ];
  description = "Object-oriented programming realization using coroutine";
  license = stdenv.lib.licenses.bsd3;
}
