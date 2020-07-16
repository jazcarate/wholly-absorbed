{ mkDerivation, aeson, base, hspec, http-client, mockery
, monad-logger, persistent, persistent-sqlite, persistent-template
, servant, servant-client, servant-server, stdenv
, string-conversions, text, transformers, wai, warp
}:
mkDerivation {
  pname = "rapt";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base monad-logger persistent persistent-sqlite
    persistent-template servant servant-server string-conversions text
    transformers wai warp
  ];
  executableHaskellDepends = [
    aeson base monad-logger persistent persistent-sqlite
    persistent-template servant servant-server string-conversions text
    transformers wai warp
  ];
  testHaskellDepends = [
    aeson base hspec http-client mockery monad-logger persistent
    persistent-sqlite persistent-template servant servant-client
    servant-server string-conversions text transformers wai warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
