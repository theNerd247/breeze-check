{stdenv, elm}: 

stdenv.mkDerivation {
  name = "breeze-check-ui-1.0.0";
  src = ./.;
  buildInputs = [elm];

  buildPhase = ''
    export HOME=$PWD
    elm make --yes --output elm.js UI.elm

  '';

  installPhase = ''
    mkdir -p $out/js
    cp elm.js $out/js/elm.js
  '';
}