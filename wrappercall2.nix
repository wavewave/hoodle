{ pkgs ? import <nixpkgs> {} }:

with pkgs;

import ./wrapper2.nix { inherit pkgs stdenv haskellPackages gtk3 poppler makeWrapper wrapGAppsHook gnome3 pkgconfig ;
                       packages = self : [] ; }
