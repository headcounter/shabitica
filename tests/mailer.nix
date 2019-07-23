{ common, ... }:

let
  testScript = ''
    import time
    import mailbox
    from habitipy import Habitipy

    def fetch_new_mail():
      box = mailbox.Maildir('/var/mail/root')
      while len(box) == 0:
        time.sleep(0.5)
      key = next(box.iterkeys())
      mail = box[key]
      box.remove(key)
      return mail

    anonymous = Habitipy({
      'url': 'http://localhost',
      'login': "",
      'password': "",
    })

    first_user = anonymous.user.auth.local.register.post(
      username='foo',
      email='foo@example.org',
      password='snakeoil',
      confirmPassword='snakeoil',
    )

    print(fetch_new_mail())

    foo = Habitipy({
      'url': 'http://localhost',
      'login': first_user['id'],
      'password': first_user['apiToken'],
    })

    reply = foo.groups.post(
      name='Testparty',
      type='party',
      privacy='private',
    )

    party_id = reply['id']

    foo.groups[party_id].invite.post(
      emails=[
        {'name': 'bar', 'email': 'bar@example.com'}
      ]
    )

    print(fetch_new_mail())

    anonymous.user.reset_password.post(email='foo@example.org')

    print(fetch_new_mail())
  '';

in {
  name = "shabitica-mailer";

  machine = { pkgs, lib, ... }: {
    imports = [ common ];
    shabitica.hostName = "localhost";

    services.postfix.enable = true;
    services.postfix.virtual = "/.*/ root\n";
    services.postfix.virtualMapType = "regexp";
    services.postfix.config = {
      inet_interfaces = "127.0.0.1";
      virtual_alias_domains = "";
    };

    environment.systemPackages = let
      inherit (pkgs.python3Packages) buildPythonApplication callPackage;
    in lib.singleton (buildPythonApplication {
      name = "mailer-test-runner";

      propagatedBuildInputs = let
        habitipy = callPackage migrations/habitipy.nix {};
      in lib.singleton habitipy;

      src = pkgs.runCommand "mailer-test-runner-source" {} ''
        mkdir -p "$out"

        # Shebang will be patched during actual package build.
        ( echo "#!/usr/bin/env python"
          echo -n ${lib.escapeShellArg testScript}
        ) > "$out/mailer-test-runner"

        cat > "$out/setup.py" <<EOF
        from setuptools import setup
        setup(
          name="mailer-test-runner",
          install_requires=["habitipy"],
          scripts=['mailer-test-runner']
        )
        EOF
      '';
    });
  };

  testScript = ''
    $machine->waitForUnit('shabitica.service');
    $machine->succeed('mailer-test-runner >&2');
  '';
}
