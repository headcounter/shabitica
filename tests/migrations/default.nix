{ common, pkgs, lib, ... }:

{
  name = "shabitica-migration";

  machine = { config, pkgs, ... }: {
    imports = [ common ];
    shabitica.hostName = "localhost";

    environment.systemPackages = let
      mainRunner = ''
        import sys
        import json
        import unittest

        from habitipy import Habitipy

        SPEC = json.load(open('${./spec.json}'))

        class UserSpec:
          def __init__(self, spec, api_version):
            for name, value in spec.items():
              setattr(self, name, value)
            apiver = ['api', 'v' + str(api_version)]
            self.api = Habitipy({
              'url': 'http://localhost',
              'login': self.apiUser,
              'password': self.apiToken,
            }, current=apiver)

        class MigrationTest(unittest.TestCase):
          def getuser(self, name, api_version=3):
            return UserSpec(SPEC[name], api_version)

          def runTest(self):
            scopevars = {key: getattr(self, key) for key in dir(self)}
            exec(open(sys.argv[1]).read(), scopevars)

        if __name__ == '__main__':
          result = unittest.TextTestRunner(verbosity=0).run(MigrationTest())
          sys.exit(not result.wasSuccessful())
      '';

      testRunner = pkgs.python3Packages.buildPythonApplication {
        name = "migration-test-runner";

        propagatedBuildInputs = let
          habitipy = pkgs.python3Packages.callPackage ../../pkgs/habitipy {
            shabiticaSource = config.shabitica.packages.source;
          };
        in lib.singleton habitipy;

        src = pkgs.runCommand "migration-test-runner-source" {} ''
          mkdir -p "$out"

          # Shebang will be patched during actual package build.
          ( echo "#!/usr/bin/env python"
            echo -n ${lib.escapeShellArg mainRunner}
          ) > "$out/migration-test-runner"

          cat > "$out/setup.py" <<EOF
          from setuptools import setup
          setup(
            name="migration-test-runner",
            install_requires=["habitipy"],
            scripts=['migration-test-runner']
          )
          EOF
        '';
      };
    in lib.singleton testRunner;
  };

  testScript = let
    mkCmd = lib.concatMapStringsSep " " lib.escapeShellArg;

    mkTest = { file, testScript, ... }: let
      mkPythonStr = str: "'${lib.escape ["\\" "'"] str}'";
      testFile = pkgs.writeText "test-script.py" testScript;
    in ''
      with machine.nested(${mkPythonStr "testing migration ${file}"}):
        machine.succeed('migration-test-runner ${testFile}')
    '';

    inherit (pkgs) writeText;
    inherit (import ../../pkgs/shabitica/docinfo.nix) migrationMsg;

    emptyArmoire = writeText "empty-armoire.js" ''
      db.users.updateOne({'auth.local.username': 'foo'},
                         {$set: {'flags.armoireEmpty': true}});
      quit(db.users.count({'flags.armoireEmpty': true}) == 1 ? 0 : 1);
    '';

  in ''
    # fmt: off
    machine.wait_for_unit('shabitica.service')

    with machine.nested('populate database with old version'):
      machine.stop_job('shabitica.service')
      machine.succeed('shabitica-db-shell --eval "db.dropDatabase()"')
      machine.succeed('shabitica-db-restore --db admin ${./fixture}')
      machine.succeed('rm /var/lib/shabitica/db-version')

    with machine.nested('reboot to run migrations'):
      machine.shutdown()
      machine.wait_for_unit('shabitica.service')

    ${lib.concatMapStrings mkTest (import ../../pkgs/shabitica/migrations.nix)}

    with machine.nested('reboot machine to hopefully not run migrations'):
      machine.shutdown()
      machine.wait_for_unit('shabitica.service')

    with machine.nested('verify that migrations were not applied again'):
      # Note that we do not use machine.fail(), because this won't fail our
      # whole test if there is an unrelated error. So instead we use positive
      # matching with grep and use "wc -l" to check if the amount of matching
      # lines is indeed zero.
      machine.succeed(
        'test "$(journalctl -b -u shabitica-db-update.service'
        ' | grep -F "${migrationMsg}" | wc -l)" -eq 0'
      )

    with machine.nested('check whether Armoire is correctly restocked'):
      machine.succeed('shabitica-db-shell < ${emptyArmoire}')
      machine.succeed('echo foo | sha256sum | cut -d" " -f1 '
                      '> /var/lib/shabitica/armoire.sha256')

      with machine.nested('reboot to run migrations'):
        machine.shutdown()
        machine.wait_for_unit('shabitica.service')

      machine.succeed('shabitica-db-shell < ${writeText "check-armoire.js" ''
        quit(db.users.count({'flags.armoireEmpty': true}));
      ''}')

    with machine.nested('ensure that Armoire is not unnecessarily restocked'):
      machine.succeed('shabitica-db-shell < ${emptyArmoire}')

      with machine.nested('reboot to run migrations'):
        machine.shutdown()
        machine.wait_for_unit('shabitica.service')

      machine.succeed('shabitica-db-shell < ${writeText "check-armoire.js" ''
        quit(db.users.count({'flags.armoireEmpty': true}) == 1 ? 0 : 1);
      ''}')
  '';
}
