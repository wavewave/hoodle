{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let wrapper = import ./wrapper.nix { inherit stdenv haskellPackages gtk3 poppler makeWrapper;
                                     packages = self : [] ; }
                                   ;

in wrapper