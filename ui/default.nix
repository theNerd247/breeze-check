{stdenv, elm}: 

stdenv.mkDerivation {
  name = "breeze-check-ui-1.0.0";
  src = ./.;
  buildInputs = [elm];

  buildPhase = ''
    elm make --output elm.js UI.elm
  '';

  installPhase = ''
    mkdir -p $out
    mv elm.js $out
  '';
}
