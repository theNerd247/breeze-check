{pkgs ? import <nixpkgs> {}}:

let 
  ghc = 
    pkgs.haskellPackages.ghcWithPackages (hpkgs: 
      (import ./dev.nix).breeze-login.buildInputs
      ++ [hpkgs.cabal-install]
    );
in
  ghc
