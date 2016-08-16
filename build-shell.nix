{ pkgs ? (import <nixpkgs>{}) }:

let hsconfig = import ./default.nix { poppler = pkgs.poppler; gtk3 = pkgs.gtk3; };
    newhaskellpackages = pkgs.haskellPackages.override { overrides = hsconfig; };
    hsenv = newhaskellpackages.ghcWithPackages 
      (p: with p; [ #hoodle-builder hoodle-render hoodle-publish xournal-parser
                    #hoodle-types xournal-types coroutine-object
                    case-insensitive
                    attoparsec-conduit SHA aeson-pretty blaze-html hinotify zlib-conduit
                    http-client-tls http-conduit handa-gdata xml fsnotify json pem pureMD5 
                    regex-base regex-posix configurator http-types system-filepath
                    transformers-free websockets 
                    data-default-instances-containers asn1-parse 
                    Diff
                  ]);
 in pkgs.stdenv.mkDerivation { 
      name = "env-hoodle-build";
      propagatedBuildInputs = [ pkgs.wrapGAppsHook ] ;
      buildInputs = [ hsenv pkgs.x11 pkgs.xlibs.libXi pkgs.gtk3 pkgs.poppler pkgs.pkgconfig
                      pkgs.gd pkgs.librsvg
                    ];
      shellHook = ''
      '';

    }


