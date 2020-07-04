{ mkDerivation, base, either, free, mtl, stdenv, transformers }:
1;5002;0cmkDerivation {
  pname = "coroutine-object";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [ base either free mtl transformers ];
  description = "Object-oriented programming realization using coroutine";
  license = stdenv.lib.licenses.bsd3;
}
