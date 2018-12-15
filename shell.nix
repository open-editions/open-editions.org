{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
