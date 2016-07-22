{ pkgs ? import <nixpkgs> {} }:

with pkgs;

import ./wrapper.nix { inherit stdenv haskellPackages gtk3 poppler makeWrapper;
                       packages = self : [] ; }
