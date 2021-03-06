let

  config = {
    packageOverrides = pkgs: rec {

      haskellPackages = pkgs.haskellPackages.override {
        overrides = new: old: rec {

          breeze-login = new.callPackage ./default.nix {};

          breeze-login-minimal = pkgs.haskell.lib.overrideCabal (
            pkgs.haskell.lib.justStaticExecutables (
              new.callPackage ./default.nix { }
            )
          )
          ( old: { enableSharedExecutables = false; });

          heist = pkgs.haskell.lib.dontCheck old.heist;

          snap = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal old.snap (
            oldSnap: {
              configureFlags = "-f openssl";
            }
          ));

          elm-export = old.callPackage ./nix/elm-export.nix {};

          inherit (pkgs.callPackage ./nix/simple.nix {})
            simple-core
            simple-aeson
            simple-string
            simple-snap;

        };
      };
    };
  };

  pkgs = import <nixpkgs> {inherit config;};

in
{breeze-login = pkgs.haskellPackages.breeze-login;
 breeze-login-minimal = pkgs.haskellPackages.breeze-login-minimal;
}
