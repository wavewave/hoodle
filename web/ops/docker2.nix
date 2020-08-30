{ pkgs ? import ../../nix/pinnedNixpkgs.nix  }:

with pkgs;

let

  hoodle-static = (import ../release.nix {}).hoodle-static;  # (callPackage ./hoodle.nix {}).hoodle-static;

  NB_USER = "wavewave";
  NB_UID = "1000";
  dockerEtc = runCommand "docker-etc" {} ''
    mkdir -p $out/etc/pam.d
    echo "root:x:0:0::/root:/bin/sh" > $out/etc/passwd
    echo "${NB_USER}:x:${NB_UID}:${NB_UID}::/home/${NB_USER}:" >> $out/etc/passwd
    echo "root:!x:::::::" > $out/etc/shadow
    echo "${NB_USER}:!:::::::" >> $out/etc/shadow

    echo "root:x:0:" > $out/etc/group
    echo "${NB_USER}:x:${NB_UID}:" >> $out/etc/group
    echo "root:x::" > $out/etc/gshadow
    echo "${NB_USER}:!::" >> $out/etc/gshadow
  '';

in

dockerTools.buildLayeredImage {
  name = "hoodle-webserver-docker";
  tag = "latest";
  contents = [
    dockerEtc
    bashInteractive
    coreutils
    nodePackages.http-server
    hoodle-static
  ];
  config = {
    Cmd = ["${nodePackages.http-server}/bin/http-server" "-p" "8080"];
    User = NB_USER;
    WorkingDir = "${hoodle-static}";
  };
  extraCommands = ''
    mkdir -m 1777 ./tmp
    mkdir -m 777 -p ./home/${NB_USER}
  '';
  maxLayers= 100;
}
