let
  pkgs = import <nixpkgs> {};
in

{breeze-ui = 
  pkgs.callPackage ./default.nix { 
    elm = pkgs.elmPackages.elm;
  };
}
