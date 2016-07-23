{ mkDerivation, base, cmdargs, configurator, containers, directory
, filepath, hoodle-core, mtl, stdenv, dyre
}:
mkDerivation {
  pname = "hoodle";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cmdargs configurator containers directory filepath hoodle-core
    mtl dyre
  ];
  configureFlags = [ "-fdyre" ];
  executableHaskellDepends = [ base cmdargs hoodle-core ];
  homepage = "http://ianwookim.org/hoodle";
  description = "Executable for hoodle";
  license = stdenv.lib.licenses.gpl3;
}
