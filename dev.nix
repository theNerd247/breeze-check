let 
  pkgs = import <nixpkgs> {};
in
  
{breeze-check = pkgs.stdenv.callPackage ./. {
    stdenv = pkgs.stdenv;
    breeze-login = (import breeze-login/dev.nix).breeze-login;
    breeze-ui = (import ui/dev.nix).ui;
  };
}
