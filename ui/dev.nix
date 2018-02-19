let
  pkgs = import <nixpkgs> {};
in

{ui = pkgs.callPackage ./default.nix {};
}
