{ common, pkgs, lib, ... }:

{
  name = "habitica-migration";

  machine = { pkgs, ... }: {
    imports = [ common ];
    habitica.hostName = "localhost";

    environment.systemPackages = let
      mainRunner = ''
        import sys
        import json
        import unittest

        from habitipy import Habitipy

        SPEC = json.load(open('${./spec.json}'))

        class UserSpec:
          def __init__(self, spec):
            for name, value in spec.items():
              setattr(self, name, value)
            self.api = Habitipy({
              'url': 'http://localhost',
              'login': self.apiUser,
              'password': self.apiToken,
            })

        class MigrationTest(unittest.TestCase):
          def getuser(self, name):
            return UserSpec(SPEC[name])

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
          habitipy = pkgs.python3Packages.callPackage ./habitipy.nix {};
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

    inherit (import ../../docinfo.nix) migrationMsg;

  in ''
    $machine->waitForUnit('habitica.service');

    $machine->nest('populate database with old version', sub {
      $machine->stopJob('habitica.service');
      $machine->succeed('habitica-db-shell --eval "db.dropDatabase()"');
      $machine->succeed('habitica-db-restore --db admin ${./fixture}');
      $machine->succeed('rm /var/lib/habitica/db-version');
    });

    $machine->nest('reboot to run migrations', sub {
      $machine->shutdown;
      $machine->waitForUnit('habitica.service');
    });

    ${lib.concatMapStrings mkTest (import ../../migrations.nix)}

    $machine->nest('reboot machine to hopefully not run migrations', sub {
      $machine->shutdown;
      $machine->waitForUnit('habitica.service');
    });

    $machine->nest('verify that migrations were not applied again', sub {
      # Note that we do not use $machine->fail, because this won't fail our
      # whole test if there is an unrelated error. So instead we use positive
      # matching with grep and use "wc -l" to check if the amount of matching
      # lines is indeed zero.
      $machine->succeed(
        'test "$(journalctl -b -u habitica-db-update.service'.
        ' | grep -F "${migrationMsg}" | wc -l)" -eq 0'
      );
    });
  '';
}
