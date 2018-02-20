{stdenv, breeze-login, breeze-ui}:

stdenv.mkDerivation {
  name = "breeze-check-1.0.0";
  buildInputs = [breeze-login breeze-ui];
}
