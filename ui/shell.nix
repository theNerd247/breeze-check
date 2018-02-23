#let 
  #pkgs = import <nixpkgs> {};
#in
#pkgs.stdenv.mkDerivation {
  #name = "ui-shell";
  #src=./.;
  #buildInputs = [
    (import ./dev.nix).breeze-ui
    #pkgs.elmPackages.elm-oracle 
    #pkgs.elmPackages.elm-test :
    #pkgs.elmPackages.elm-format
  #];
#}
