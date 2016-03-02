{ pkgs ? import <nixpkgs> {} }:

let lib = pkgs.haskell.lib;
    extend = rattrs: f: self: let super = rattrs self; in super // f self super;
    fix = f: let x = f x // { __unfix__ = f; }; in x;

    ghc = pkgs.haskell-ng.packages.ghc;
    ghcjs = pkgs.haskell-ng.packages.ghcjs;
    stdenv = pkgs.stdenv;
    

    conf = self: super: {
             hoodlehub = lib.addBuildTool super.hoodlehub ghc.cabal-install;
             hoodlehub-ghcjs = lib.addBuildTools super.hoodlehub-ghcjs [ghc.ghc ghc.cabal-install];
           }; 
in fix (extend hoodlehubpkgs conf)


