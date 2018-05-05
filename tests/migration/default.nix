{ common, pkgs, lib, ... }:

{
  name = "habitica-migration";

  machine = { pkgs, ... }: {
    imports = [ common ];
    habitica.insecureDB = true;
    habitica.hostName = lib.mkForce "localhost";
    systemd.services.habitica-reset-schema-version = {
      description = "Reset Habitica Database Schema Version";
      requiredBy = [ "habitica.service" ];
      after = [ "habitica-statedir-init.service" ];
      before = [ "habitica-db.service" ];
      script = "rm /var/lib/habitica/db-version";
      serviceConfig.Type = "oneshot";
    };
    systemd.services.habitica-prepopulate-db = {
      description = "Prepopulate Habitica Database";
      requiredBy = [ "habitica.service" ];
      after = [ "habitica-db.service" "habitica-init.service" ];
      before = [ "habitica.service" "habitica-db-update.service" ];

      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      serviceConfig.User = "habitica";
      serviceConfig.Group = "habitica";
      serviceConfig.PrivateTmp = true;

      script = ''
        ${pkgs.mongodb-tools}/bin/mongorestore --db admin ${./fixture}
      '';
    };

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

  in ''
    startAll;
    $machine->waitForUnit('habitica.service');
  '' + lib.concatMapStrings mkTest (import ../../migrations.nix);
}
