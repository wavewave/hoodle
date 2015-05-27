{ pkgs ? import <nixpkgs> {} }:

let newghcpkgs = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules> {
      ghc = pkgs.haskell.compiler.ghc7101;
      packageSetConfig = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules/configuration-ghc-7.10.x.nix> {};
      overrides = self: super: rec { 
        coroutine-object = self.callPackage ./coroutine-object {};
        xournal-types = self.callPackage ./xournal-types {};
        xournal-parser = self.callPackage ./xournal-parser { inherit xournal-types; };
        hoodle-types = self.callPackage ./hoodle-types {};
        hoodle-builder = self.callPackage ./hoodle-builder { inherit hoodle-types; };
        hoodle-parser = self.callPackage ./hoodle-parser { inherit hoodle-types xournal-types; };
        hoodle-render = self.callPackage ./hoodle-render { inherit hoodle-types; };
        hoodle-publish = self.callPackage ./hoodle-publish { inherit hoodle-types hoodle-render; };
        hoodle-core = self.callPackage ./hoodle-core { inherit coroutine-object hoodle-types hoodle-parser hoodle-builder hoodle-render hoodle-publish xournal-parser; };
        hoodle = self.callPackage ./hoodle { inherit hoodle-core; };
      };
    };
in with newghcpkgs; { 
     inherit coroutine-object xournal-types xournal-parser hoodle-types hoodle-builder hoodle-parser hoodle-render hoodle-publish hoodle-core hoodle;
   }
