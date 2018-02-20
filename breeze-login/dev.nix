let
  config = {
    packageOverrides = pkgs: rec {

      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: rec {

          breeze-login = new.callPackage ./default.nix {};

          snap = old.snap.override {
            heist = pkgs.haskell.lib.dontCheck old.heist;
          };

          simple = new.callPackage ../../simple/simple {};

          simple-aeson = 
            new.callPackage ../../simple/simple-aeson { };

          simple-string = 
            new.callPackage ../../simple/simple-string { };

          simple-snap = 
            new.callPackage ../../simple/simple-snap { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> {inherit config;};

in
{breeze-login = pkgs.haskellPackages.breeze-login;
}
