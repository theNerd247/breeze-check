let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: rec {

          breeze-login = 
          let
            simple = 
              pkgs.haskellPackages.callPackage ../simple/simple/default.nix {};

            snap = old.snap.override {
              heist = pkgs.haskell.lib.dontCheck old.heist;
            };
          in
            pkgs.haskellPackages.callPackage ./default.nix {
              simple = simple;
              snap = snap;
              simple-aeson = 
                pkgs.haskellPackages.callPackage ../simple/simple-aeson/default.nix {
                  simple = simple;
                };
              simple-string = 
                pkgs.haskellPackages.callPackage ../simple/simple-string/default.nix {
                  simple = simple;
                };
              simple-snap = 
                pkgs.haskellPackages.callPackage ../simple/simple-snap/default.nix { 
                  snap = snap; 
                  simple = simple; 
                };
            };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
{breeze-login = pkgs.haskellPackages.breeze-login;
}
