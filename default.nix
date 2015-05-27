{ pkgs ? import <nixpkgs> {}
, stdenv
, ghc
}:

self:
rec { 
      coroutine-object = ghc.callPackage ./coroutine-object {};

      xournal-types    = ghc.callPackage ./xournal-types {};

      xournal-parser   = ghc.callPackage ./xournal-parser { 
                           xournal-types = self.xournal-types; 
                         };

      hoodle-types     = ghc.callPackage ./hoodle-types {};

      hoodle-builder   = ghc.callPackage ./hoodle-builder { 
                           hoodle-types = self.hoodle-types; 
                         };

      hoodle-parser    = ghc.callPackage ./hoodle-parser { 
                           hoodle-types = self.hoodle-types; 
                           xournal-types = self.xournal-types; 
                         };

      hoodle-render    = ghc.callPackage ./hoodle-render { 
                           hoodle-types = self.hoodle-types; 
                         };

      hoodle-publish   = ghc.callPackage ./hoodle-publish { 
                           hoodle-types = self.hoodle-types; 
                           hoodle-render = self.hoodle-render; 
                         };

      hoodle-core      = ghc.callPackage ./hoodle-core { 
                           coroutine-object = self.coroutine-object;
                           hoodle-types = self.hoodle-types; 
                           hoodle-parser = self.hoodle-parser; 
                           hoodle-builder = self.hoodle-builder;
                           hoodle-render = self.hoodle-render; 
                           hoodle-publish = self.hoodle-publish;
                           xournal-parser = self.xournal-parser; 
                         };
      hoodle           = ghc.callPackage ./hoodle { 
                           hoodle-core = self.hoodle-core; 
                         };
}
