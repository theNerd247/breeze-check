{stdenv, elm}: 

stdenv.mkDerivation rec {
  version="1.0.3";
  name = "breeze-check-ui-${version}";
  src = ./.;
  buildInputs = [elm];

  buildPhase = ''
    export HOME=$PWD
    elm make --yes --output elm.js UI.elm

  '';

  installPhase = ''
    mkdir -p $out/js
    cp elm.js $out/js/elm.$version.js
  '';
}
