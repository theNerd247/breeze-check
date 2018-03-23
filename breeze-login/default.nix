{ mkDerivation, aeson, aeson-lens, base, bytestring, data-default
, elm-export, exceptions, fast-logger, http-conduit, ixset, lens
, mtl, simple-aeson, simple-core, simple-snap, simple-string, snap
, snap-core, stdenv, stm, text, time, transformers
}:
mkDerivation {
  pname = "breeze-login";
  version = "1.3.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-lens base bytestring data-default elm-export exceptions
    fast-logger http-conduit ixset lens mtl simple-aeson simple-core
    simple-snap simple-string snap snap-core stm text time transformers
  ];
  homepage = "https://github.com/githubuser/breeze-login#readme";
  license = stdenv.lib.licenses.bsd3;
  postBuild = ''
    mkdir -p $out
    cp -r --no-preserve=mode ./snaplets $out/snaplets
  '';
}
