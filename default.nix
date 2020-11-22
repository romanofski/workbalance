{ compiler ? null, nixpkgs ? null, with-icu ? false }:

with (import .nix/nixpkgs.nix { inherit compiler nixpkgs; });

let
  envPackages = self: [ self.workbalance];
  env = haskellPackages.ghcWithPackages envPackages;
  nativeBuildTools = with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    ghcid
    hlint
    doctest
  ];
in
if pkgs.lib.inNixShell
then haskellPackages.shellFor {
  withHoogle = true;
  packages = haskellPackages: [ haskellPackages.workbalance ];
  nativeBuildInputs = haskellPackages.workbalance.env.nativeBuildInputs ++ nativeBuildTools;
}
else {
  workbalance = pkgs.stdenv.mkDerivation {
    name = "workbalance-with-packages-${env.version}";
    nativeBuildInputs = [ pkgs.makeWrapper ];
    # This creates a Bash script, which sets the GHC in order for dyre to be
    # able to build the config file.
    buildCommand = ''
      mkdir -p $out/bin
      makeWrapper ${env}/bin/workbalance $out/bin/workbalance \
      --set NIX_GHC "${env}/bin/ghc"
    '';
    preferLocalBuild = true;
    allowSubstitutes = false;
  };
}
