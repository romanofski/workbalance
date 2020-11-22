{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc884" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        workbalance = hsuper.callPackage ./workbalance.nix { };
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    # nixpkgs master - 2020-11-21
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/b5613e78fd8431af680a00c0cdd42a0637601c3a.tar.gz";
      sha256 = "1fn8j4r7hx3bl1amdwmw9lyccjci891igrf9pz3vgkfq1bpmqd65";
    }
    else
    nixpkgs;
in
import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
