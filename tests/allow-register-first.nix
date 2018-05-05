{ common, lib, registerUser, ... }:

{
  name = "habitica-allow-register-first";

  nodes.habitica = common;
  nodes.client = {};

  testScript = ''
    startAll;

    $habitica->waitForUnit('habitica.service');
    $habitica->waitForOpenPort(80);
    $client->waitForUnit('multi-user.target');

    subtest "check if service only allows first user to register", sub {
      $client->succeed(${registerUser "foo" "habitica"});
      $client->fail(${registerUser "bar" "habitica"});
    };
  '';
}
