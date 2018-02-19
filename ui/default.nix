{stdenv, elmPackages}: 

stdenv.mkDerivation {
  name = "breeze-check-ui-1.0.0";
  src = ./.;
  buildInputs = [elmPackages.elm];

  buildPhase = ''
    elm make --output elm.js UI.elm
  '';

  installPhase = ''
    mkdir $out
    mv elm.js $out
  '';
}
