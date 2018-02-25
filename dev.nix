{ pkgs ? import <nixpkgs> {} }:
  
{breeze-check = import ./default.nix {
    inherit (pkgs) buildEnv;
    breeze-login = (import ./breeze-login/dev.nix).breeze-login-minimal;
    inherit (import ./ui/dev.nix) breeze-ui;
  };
}
