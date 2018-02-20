with import <nixpkgs> {};

{breeze-ui = 
  pkgs.callPackage ./default.nix { 
    elm = pkgs.elmPackages.elm;
  };
}
