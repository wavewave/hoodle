{
  pkgs,
  haskellLib,
}: rec {
  haskellOverlayWebClient = hself: hsuper: {
    coroutine-object =
      hself.callCabal2nix "coroutine-object" ./coroutine-object {};
    hoodle-util = hself.callCabal2nix "hoodle-util" ./util {};
    comonad = haskellLib.dontCheck hsuper.comonad;
    semigroupoids = haskellLib.dontCheck hsuper.semigroupoids;
    QuickCheck = haskellLib.dontCheck hsuper.QuickCheck;
    scientific = haskellLib.dontCheck hsuper.scientific;
    tasty-quickcheck = haskellLib.dontCheck hsuper.tasty-quickcheck;
    time-compat = haskellLib.dontCheck hsuper.time-compat;
  };
  hpkgsWebClient =
    pkgs.haskell.packages.ghcjs.extend haskellOverlayWebClient;

  mkWebShellFor = compiler: let
    hsenvWebServer = pkgs.haskell.packages.${compiler}.ghcWithPackages (p: [
      p.acid-state
      p.microlens
      p.microlens-th
      p.monad-loops
      p.servant
      p.servant-server
      p.websockets
    ]);
    hsenvWebClient = hpkgsWebClient.ghcWithPackages (p: []);
  in
    pkgs.mkShell {
      name = "hoodle-web-shell";
      buildInputs = [
        pkgs.nodePackages.http-server
        hsenvWebServer
        hsenvWebClient
        pkgs.cabal-install
        pkgs.ormolu
      ];
    };

  supportedCompilersWeb = ["ghc8107"];
  defaultCompilerWeb = "ghc8107";
}
