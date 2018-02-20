let 
  pkgs = import <nixpkgs> {};
in
  
{breeze-check = import ./default.nix {
    inherit (pkgs) buildEnv;
    inherit (import ./breeze-login/dev.nix) breeze-login;
    inherit (import ./ui/dev.nix) breeze-ui;
  };
}
