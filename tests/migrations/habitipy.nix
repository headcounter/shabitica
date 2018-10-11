{ buildPythonPackage, lib, fetchFromGitHub
, plumbum, requests, aiohttp, responses, hypothesis

, shabiticaSource ? null
}:

buildPythonPackage rec {
  pname = "habitipy";
  version = "0.2.2";

  src = fetchFromGitHub {
    owner = "ASMfreaK";
    repo = "habitipy";
    rev = "v${version}";
    sha256 = "1kmkiryyfr4r5p2qgspg5raxkivl1yahyb0v42wryn4s93hv83qf";
  };

  patches = lib.singleton ../../patches/habitipy.patch;

  # Update the API documentation to the current version of the Shabitica source
  # code so that we have all available API endpoints.
  postPatch = let
    apiPath = "${shabiticaSource}/website/server/controllers";
  in lib.optionalString (shabiticaSource != null) ''
    find ${lib.escapeShellArg apiPath} \
      -type f -exec sed -ne 's/^.*\(@api\)/\1/; T; s/  / /g; p' {} + \
      > habitipy/apidoc.txt
  '';

  propagatedBuildInputs = [ plumbum requests ];

  doCheck = lib.versionAtLeast (lib.getVersion responses) "0.5.0";
  preCheck = "export HOME=\"$PWD\"";
  checkInputs = [ aiohttp responses hypothesis ];
}
