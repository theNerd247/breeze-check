{stdenv, elm}: 

stdenv.mkDerivation rec {
  version="1.1.4";
  name = "breeze-check-ui-${version}";
  src = ./.;
  buildInputs = [elm];

  buildPhase = ''
    export HOME=$PWD
    $src/build.sh ${version}
  '';

  installPhase = ''
    mkdir -p $out/js
    cp elm*.*.js $out/js/
  '';
}
