{ common, lib, registerUser, ... }:

{
  name = "shabitica-allow-register-first";

  nodes.shabitica = common;
  nodes.client = {};

  testScript = ''
    startAll;

    $shabitica->waitForUnit('shabitica.service');
    $shabitica->waitForOpenPort(80);
    $client->waitForUnit('multi-user.target');

    subtest "check if service only allows first user to register", sub {
      $client->succeed(${registerUser "foo" "shabitica"});
      $client->fail(${registerUser "bar" "shabitica"});
    };
  '';
}
