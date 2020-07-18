{ mkDerivation, aeson, base, esqueleto, hspec, http-client, mockery
, monad-logger, mtl, persistent, persistent-sqlite
, persistent-template, servant, servant-client, servant-server
, stdenv, string-conversions, text, time, transformers, wai, warp
, wrap
}:
mkDerivation {
  pname = "rapt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base esqueleto monad-logger mtl persistent persistent-sqlite
    persistent-template servant servant-server string-conversions text
    time transformers wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base hspec http-client mockery servant servant-client text wrap
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
