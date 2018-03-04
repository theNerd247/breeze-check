{stdenv, elm}: 

stdenv.mkDerivation rec {
  version="1.0.5";
  name = "breeze-check-ui-${version}";
  src = ./.;
  buildInputs = [elm];

  buildPhase = ''
    export HOME=$PWD
    $src/build.sh
  '';

  installPhase = ''
    mkdir -p $out/js
    cp elm.js $out/js/elm.$version.js
  '';
}
