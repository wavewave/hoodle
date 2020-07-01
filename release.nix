{ pkgs ? import ./nix/pinnedNixpkgs.nix }:

with pkgs;

let
    deps = callPackage ./deps.nix {};
    hsconfig = import ./config.nix {
      inherit deps;
      inherit haskell;
      poppler = pkgs.poppler;
      gtk3 = pkgs.gtk3;
    };
    newHsPkgs = haskell.packages.ghc865.override { overrides = hsconfig; };

in

with newHsPkgs;
{
  inherit microlens coroutine-object xournal-types xournal-parser hoodle-types hoodle-builder hoodle-parser hoodle-render hoodle-publish hoodle-core hoodle;
}
