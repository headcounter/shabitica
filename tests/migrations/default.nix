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
      mkPerlStr = str: "'${lib.escape ["\\" "'"] str}'";
      testFile = pkgs.writeText "test-script.py" testScript;
    in ''
      $machine->nest(${mkPerlStr "testing migration ${file}"}, sub {
        $machine->succeed('migration-test-runner ${testFile}');
      });
    '';

    inherit (pkgs) writeText;
    inherit (import ../../docinfo.nix) migrationMsg;

    emptyArmoire = writeText "empty-armoire.js" ''
      db.users.updateOne({'auth.local.username': 'foo'},
                         {$set: {'flags.armoireEmpty': true}});
      quit(db.users.count({'flags.armoireEmpty': true}) == 1 ? 0 : 1);
    '';

  in ''
    $machine->waitForUnit('shabitica.service');

    $machine->nest('populate database with old version', sub {
      $machine->stopJob('shabitica.service');
      $machine->succeed('shabitica-db-shell --eval "db.dropDatabase()"');
      $machine->succeed('shabitica-db-restore --db admin ${./fixture}');
      $machine->succeed('rm /var/lib/shabitica/db-version');
    });

    $machine->nest('reboot to run migrations', sub {
      $machine->shutdown;
      $machine->waitForUnit('shabitica.service');
    });

    ${lib.concatMapStrings mkTest (import ../../pkgs/shabitica/migrations.nix)}

    $machine->nest('reboot machine to hopefully not run migrations', sub {
      $machine->shutdown;
      $machine->waitForUnit('shabitica.service');
    });

    $machine->nest('verify that migrations were not applied again', sub {
      # Note that we do not use $machine->fail, because this won't fail our
      # whole test if there is an unrelated error. So instead we use positive
      # matching with grep and use "wc -l" to check if the amount of matching
      # lines is indeed zero.
      $machine->succeed(
        'test "$(journalctl -b -u shabitica-db-update.service'.
        ' | grep -F "${migrationMsg}" | wc -l)" -eq 0'
      );
    });

    $machine->nest('check whether Armoire is correctly restocked', sub {
      $machine->succeed('shabitica-db-shell < ${emptyArmoire}');
      $machine->succeed('echo foo | sha256sum | cut -d" " -f1 '.
                        '> /var/lib/shabitica/armoire.sha256');
      $machine->nest('reboot to run migrations', sub {
        $machine->shutdown;
        $machine->waitForUnit('shabitica.service');
      });
      $machine->succeed('shabitica-db-shell < ${writeText "check-armoire.js" ''
        quit(db.users.count({'flags.armoireEmpty': true}));
      ''}');
    });

    $machine->nest('ensure that Armoire is not unnecessarily restocked', sub {
      $machine->succeed('shabitica-db-shell < ${emptyArmoire}');
      $machine->nest('reboot to run migrations', sub {
        $machine->shutdown;
        $machine->waitForUnit('shabitica.service');
      });
      $machine->succeed('shabitica-db-shell < ${writeText "check-armoire.js" ''
        quit(db.users.count({'flags.armoireEmpty': true}) == 1 ? 0 : 1);
      ''}');
    });
  '';
}
