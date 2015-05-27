{ pkgs ? import <nixpkgs> {}
, stdenv
, ghc
}:

self:
rec { 
      coroutine-object = ghc.callPackage ./coroutine-object {};
      xournal-types    = ghc.callPackage ./xournal-types {};
      xournal-parser   = ghc.callPackage ./xournal-parser { inherit xournal-types; };
      hoodle-types     = ghc.callPackage ./hoodle-types {};
      hoodle-builder   = ghc.callPackage ./hoodle-builder { inherit hoodle-types; };
      hoodle-parser    = ghc.callPackage ./hoodle-parser { inherit hoodle-types xournal-types; };
      hoodle-render    = ghc.callPackage ./hoodle-render { inherit hoodle-types; };
      hoodle-publish   = ghc.callPackage ./hoodle-publish { inherit hoodle-types hoodle-render; };
      hoodle-core      = ghc.callPackage ./hoodle-core { inherit hoodle-types hoodle-parser hoodle-builder hoodle-render hoodle-publish xournal-parser; };
      hoodle           = ghc.callPackage ./hoodle { inherit hoodle-core; };
}
