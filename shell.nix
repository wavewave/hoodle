{ pkgs ?
import (builtins.fetchTarball {
    name = "nixos-20.03";
    url = "https://github.com/nixos/nixpkgs/archive/5272327b81ed355bbed5659b8d303cf2979b6953.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {}
}:

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

    hoodlePkgs = [
      "coroutine-object"
      "xournal-types"
      "xournal-parser"
      "hoodle-types"
      "hoodle-builder"
      "hoodle-parser"
      "hoodle-render"
      "hoodle-phublish"
      "hoodle-core"
      "hoodle"
    ];

    pname = lib.attrByPath [ "pname" ] null;

    removeHoodlePkgsFromDep =
      ps: lib.filter (q: let n = pname q; in !(lib.any (k: k == n) hoodlePkgs)) ps;

    hsenv = newHsPkgs.ghcWithPackages
      (ps: with ps;
        let deps = builtins.concatMap
                     (p: p.buildInputs ++ p.nativeBuildInputs ++ p.propagatedBuildInputs)
                     [ coroutine-object
                       xournal-types
                       xournal-parser
                       hoodle-types
                       hoodle-builder
                       hoodle-parser
                       hoodle-render
                       hoodle-publish
                       hoodle-core
                       hoodle
                     ];
        in #removeHoodlePkgsFromDep deps
           [ #gtk3
             #cairo
             #glib
             poppler
             #gtk2hs-buildtools
  case-insensitive
                    attoparsec-conduit SHA aeson-pretty blaze-html hinotify zlib-conduit
                    http-client-tls http-conduit xml fsnotify json pem pureMD5
                    regex-base regex-posix configurator http-types system-filepath
                    transformers-free websockets
                    data-default-instances-containers asn1-parse
                    Diff
                    free strict microlens double-conversion uuid svgcairo monad-loops
                    lens gd errors either
                    #pdf-toolbox-core
                    pdf-toolbox-document
                    directory-tree
                    HTTP xml-types xml-conduit TypeCompose
           ]
      );

in

mkShell {
  buildInputs = [ hsenv cabal-install ];
}


 #     (p: with p; [ hoodle-builder hoodle-render hoodle-publish xournal-parser
 #                   hoodle-types xournal-types coroutine-object case-insensitive
 #                   attoparsec-conduit SHA aeson-pretty blaze-html hinotify zlib-conduit
 #                   http-client-tls http-conduit xml fsnotify json pem pureMD5
 #                   regex-base regex-posix configurator http-types system-filepath
 #                   transformers-free websockets
 #                   data-default-instances-containers asn1-parse
 #                   Diff
 #                 ]);
 #in pkgs.stdenv.mkDerivation {
 #     name = "env-hoodle-build";
 #     buildInputs = [ hsenv pkgs.haskellPackages.cabal-install pkgs.x11 pkgs.xlibs.libXi pkgs.gtk3 pkgs.poppler pkgs.pkgconfig
 #                   ];
 #     shellHook = ''
 #     '';
 #   }
