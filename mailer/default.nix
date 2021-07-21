{ lib, mkDerivation, aeson, base, containers, hspec, http-types
, mime-mail, neat-interpolation, network, stache, systemd
, text, unordered-containers, vector, wai, warp, word-wrap
}:
mkDerivation {
  pname = "shabitica-mailer";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers http-types mime-mail network stache systemd
    text unordered-containers vector wai warp word-wrap
  ];
  testHaskellDepends = [ hspec neat-interpolation ];
  description = "Shabitica email helper";
  license = lib.licenses.gpl3;
}
