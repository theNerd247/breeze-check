with import <nixpkgs> {};

stdenv.mkDerivation {
  name="breeze-check";
  buildInputs = [
    stack
    ghc
    elmPackages.elm
    zlib
    libcxx
    icu
    gmp
    gcc
    ncurses
    cabal-install  
  ];
}
