{pkgs ? import <nixpkgs> {}}:

let
  server = 
    let
      s = (import ./breeze-login/dev.nix).breeze-login;
    in
    pkgs.haskellPackages.ghcWithPackages (pkgs: 
      s.buildInputs
    );

  ui = 
    (import ./ui/shell.nix).buildInputs;
in

pkgs.stdenv.mkDerivation {
  name="breeze-shell";
  buildInputs = [server] ++ ui;
  src = ./.;
}
