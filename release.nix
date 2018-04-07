{ nixpkgs ? <nixpkgs>
}:

with import nixpkgs {}; 

let hsconfig = import ./default.nix { poppler = poppler; gtk3 = gtk3; };
    newHaskellPackages = haskellPackages.override { overrides = hsconfig; };
in with newHaskellPackages; {
     inherit coroutine-object xournal-types xournal-parser hoodle-types hoodle-builder hoodle-parser hoodle-render hoodle-publish hoodle-core hoodle;
   }
