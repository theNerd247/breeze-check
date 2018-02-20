with import ./breeze-login/dev.nix ;
with import ./ui/dev.nix ;
with import <nixpkgs> {};

#let
  #config = {
    #packageOverrides = pkgs: rec {

      #haskellPackages = pkgs.haskellPackages.override {
        #overrides = new: old: rec {
          #ghc-mod = pkgs.haskell.lib.dontCheck 
            #(pkgs.haskell.lib.dontHaddock old.ghc-mod);
        #};
      #};
    #};
  #};


  #pkgs = import <nixpkgs> { inherit config; };
#in

pkgs.stdenv.mkDerivation {
  name = "breeze-shell";
  src = ./.;
  buildInputs = 
  breeze-login.buildInputs 
  ++ breeze-ui.buildInputs;
}
