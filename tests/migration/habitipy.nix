# NOTE: If you change anything in this file, be sure it stays
# backwards-compatible with the nixpkgs revision used in
# create-fixture.nix.
#
# You can check this by issuing the following command:
#
#   nix-build --no-out-link create-fixture.nix
#
{ buildPythonPackage, lib, fetchFromGitHub
, plumbum, requests, aiohttp, responses, hypothesis
}:

buildPythonPackage rec {
  pname = "habitipy";
  version = "0.1.18";

  src = fetchFromGitHub {
    owner = "ASMfreaK";
    repo = "habitipy";
    rev = "v${version}";
    sha256 = "1p79vvr9nlsg7d4md0r0pn43snmspjr5kgdjflm87slqwrv2zmkq";
  };

  propagatedBuildInputs = [ plumbum requests ];

  doCheck = lib.versionAtLeast (lib.getVersion responses) "0.5.0";
  preCheck = "export HOME=\"$PWD\"";
  checkInputs = [ aiohttp responses hypothesis ];
}
