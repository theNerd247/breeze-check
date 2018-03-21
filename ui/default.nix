{stdenv, elm}: 

stdenv.mkDerivation rec {
  version="1.5.0";
  name = "breeze-check-ui-${version}";
  src = ./.;
  buildInputs = [elm];

  buildPhase = ''
    export HOME=$PWD
    $src/build.sh ${version}
  '';

  installPhase = ''
    mkdir -p $out/js
    cp build/elm*.*.js $out/js/
  '';
}
