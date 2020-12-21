{ common, lib, registerUser, ... }:

{
  name = "shabitica-allow-register-first";

  nodes.shabitica = common;
  nodes.client = {};

  testScript = ''
    # fmt: off
    start_all()

    shabitica.wait_for_unit('shabitica.service')
    shabitica.wait_for_open_port(80)
    client.wait_for_unit('multi-user.target')

    with subtest("check if service only allows first user to register"):
      client.succeed(${registerUser "foo" "shabitica"})
      client.fail(${registerUser "bar" "shabitica"})
  '';
}
