{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  name = "test-shell";
  buildInputs = [ nodejs ];
}
