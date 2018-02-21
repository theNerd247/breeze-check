{pkgs ? import <nixpkgs> {}}:

let
  server = (import ./breeze-login/shell.nix {});

  ui = 
    (import ./ui/shell.nix).buildInputs;
in

pkgs.stdenv.mkDerivation {
  name="breeze-shell";
  buildInputs = [server] ++ ui;
  src = ./.;
}
