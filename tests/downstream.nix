{ nixpkgs, lib, ... }:

{
  allow-register-first = import "${nixpkgs}/nixos/tests/make-test.nix" {
    name = "habitica";

    nodes.habitica = {
      imports = [ ../. ];
      networking.firewall.enable = false;
      habitica.hostName = "habitica";
      virtualisation.diskSize = 16384;
      virtualisation.memorySize = 1024;
    };

    nodes.client = {};

    testScript = let
      mkPerlString = val: "'${lib.escape ["\\" "'"] val}'";
      listToCommand = lib.concatMapStringsSep " " lib.escapeShellArg;
      registerUser = username: let
        data = builtins.toJSON {
          inherit username;
          email = "${username}@example.org";
          password = "test";
          confirmPassword = "test";
        };
        url = "http://habitica/api/v3/user/auth/local/register";
      in mkPerlString (listToCommand [
        "curl" "-f" "-H" "Content-Type: application/json" "-d" data url
      ]);
    in ''
      startAll;

      $habitica->waitForUnit('habitica.service');
      $habitica->waitForOpenPort(80);
      $client->waitForUnit('multi-user.target');

      subtest "check if service only allows first user to register", sub {
        $client->succeed(${registerUser "foo"});
        $client->fail(${registerUser "bar"});
      };
    '';
  };
}
