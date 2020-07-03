# hoodle

[![CircleCI](https://circleci.com/gh/wavewave/hoodle.svg?style=svg&circle-token=f70e4000c041516f08d6c3c0193958f0973803eb)](https://circleci.com/gh/wavewave/hoodle)

A pen notetaking program written in haskell

[![hoodle](https://img.youtube.com/vi/Z2wzpyxsVSU/0.jpg)](https://www.youtube.com/watch?v=Z2wzpyxsVSU)

Build
-----
Hoodle development recommends using nix for the build.
The build script has pinned nixpkgs at nixos 20.03 revision by default unless it is specified.
```
$ nix-build release.nix -A hoodle 
```
