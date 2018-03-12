{pkgs ? import <nixpkgs> {}}:

with import ./dev.nix;

let 
  ghc = 
    pkgs.haskellPackages.ghcWithPackages (pkgs: 
      with pkgs;
      breeze-login.buildInputs
    );
in
  ghc
