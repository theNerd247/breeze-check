let 
  pkgs = import <nixpkgs> {};
in
  
{breeze-check = pkgs.callPackage ./. {
    inherit (pkgs) buildEnv;
    inherit (import breeze-login/dev.nix) breeze-login;
    inherit (import ui/dev.nix) breeze-ui;
  };
}
