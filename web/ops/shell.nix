{ pkgs ? import ../../nix/pinnedNixpkgs.nix }:

with pkgs;

mkShell {
  buildInputs = [
    kubectl
    google-cloud-sdk
    socat
  ];
  shellHook = ''
    source ./env
  '';
}
