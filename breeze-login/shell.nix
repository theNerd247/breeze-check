{pkgs ? import <nixpkgs> {}}:

with import ./dev.nix;

let 
  ghc = 
    pkgs.haskellPackages.ghcWithPackages (pkgs: 
      with pkgs;
      breeze-login-minimal.buildInputs
    );
in
  ghc
