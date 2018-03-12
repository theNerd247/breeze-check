{ mkDerivation, base, bytestring, containers, Diff, directory
, fetchgit, formatting, hpack, hspec, hspec-core, HUnit, mtl
, QuickCheck, quickcheck-instances, servant, stdenv, text, time
, wl-pprint-text
}:
mkDerivation {
  pname = "elm-export";
  version = "0.6.0.1";
  src = fetchgit {
    url = "https://github.com/bchase/elm-export.git";
    sha256 = "0mg9hdp8qbwsrxgibcrvn8rgcvpvdy0n5w07xsf4224ws3yqfwda";
    rev = "e24f0cf3ff4ca94cdb966e16c4e472945e0398ce";
  };
  libraryHaskellDepends = [
    base bytestring containers directory formatting mtl servant text
    time wl-pprint-text
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring containers Diff hspec hspec-core HUnit QuickCheck
    quickcheck-instances text time
  ];
  preConfigure = "hpack";
  homepage = "http://github.com/krisajenkins/elm-export";
  description = "A library to generate Elm types from Haskell source";
  license = "unknown";
}
