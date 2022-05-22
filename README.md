# hoodle

![Build](https://github.com/wavewave/hoodle/actions/workflows/build.yml/badge.svg)

A pen notetaking program written in haskell

[![hoodle](https://img.youtube.com/vi/Z2wzpyxsVSU/0.jpg)](https://www.youtube.com/watch?v=Z2wzpyxsVSU)

Build and run 
-------------
Hoodle development recommends using nix for the build.
The build requires Nix >= 2.4 which has the support for flakes.

```
$ nix build .#
$ result/bin/hoodle
```

For developement, build in nix-shell and cabal:
```
$ nix develop .#
$ cabal v2-build hoodle
$ cabal v2-run hoodle
```
