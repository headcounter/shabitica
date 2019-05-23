{ nixpkgs, pkgs, lib, ... }:

let
  shabitica = pkgs.callPackages ../shabitica.nix {
    shabiticaConfig = rec {
      NODE_ENV = "test";
      SESSION_SECRET = "YOUR SECRET HERE";
      SESSION_SECRET_KEY = "12345678912345678912345678912345"
                         + "67891234567891234567891234567891";
      SESSION_SECRET_IV = "12345678912345678912345678912345";
      NODE_DB_URI = "mongodb://%2Ftmp%2Fdb.sock";
      TEST_DB_URI = NODE_DB_URI;
      BASE_URL = "http://localhost";
      ADMIN_EMAIL = "unconfigured@example.org";
      INVITE_ONLY = false;
    };
  };

  mkTest = name: { target, useDB ? false }: shabitica.mkCommonBuild {
    name = "test-${name}";
    buildTarget = "test:${target}";

    nativeBuildInputs = lib.attrValues shabitica.nodePackages.dev
                     ++ lib.singleton pkgs.mongodb;

    createHydraTestFailure = true;

    preConfigure = let
      mongoDbCfg = pkgs.writeText "mongodb.conf" (builtins.toJSON {
        net.bindIp = "/tmp/db.sock";
        processManagement.fork = false;
        storage.dbPath = "db";
        storage.engine = "ephemeralForTest";
        storage.journal.enabled = false;
      });
      cmd = "mkdir db; mongod --config ${mongoDbCfg} &> /dev/null &";
    in lib.optionalString useDB cmd;

    installPhase = ''
      mkdir -p "$out/nix-support"
      cp test-report.html "$out/report.html"
      echo "report test-report $out report.html" \
        > "$out/nix-support/hydra-build-products"
    '';
  };

  runTests = cat: lib.mapAttrs (name: mkTest "${cat}-${name}");

  # In newer nixpkgs, the pkgs attribute needs to be passed to testing.nix, so
  # in order to stay compatible with older nixpkgs, we only pass it if it is
  # required.
  testingLib = let
    mainExpr = import "${nixpkgs}/nixos/lib/testing.nix";
  in mainExpr ({
    inherit (pkgs) system;
  } // lib.optionalAttrs ((builtins.functionArgs mainExpr) ? pkgs) {
    inherit pkgs;
  });

in lib.mapAttrs runTests {
  basic = {
    sanity.target = "sanity";
    content.target = "content";
    common.target = "common";
  };

  api = {
    unit.target = "api:unit";
    unit.useDB = true;

    integration-v3.target = "api-v3:integration";
    integration-v3.useDB = true;

    integration-v4.target = "api-v4:integration";
    integration-v4.useDB = true;
  };
} // {
  client.e2e = testingLib.runInMachine {
    drv = shabitica.mkCommonBuild {
      name = "test-client-e2e";
      nativeBuildInputs = lib.attrValues shabitica.nodePackages.dev;
      createHydraTestFailure = true;
      buildProg = "npm run";
      buildTarget = "client:e2e";
      installPhase = ''
        nightwatch-html-reporter -d test/client/e2e/reports
        mkdir -p "$out/nix-support"
        cp test/client/e2e/reports/generatedReport.html "$out/report.html"
        echo "report test-report $out report.html" \
          > "$out/nix-support/hydra-build-products"
      '';
    };
    machine = {
      imports = [ ../. ];
      networking.firewall.enable = false;
      virtualisation.diskSize = 16384;
      virtualisation.memorySize = 2048;

      users.users.selenium.description = "Selenium User";

      systemd.services.selenium = {
        description = "Selenium Server";
        requiredBy = [ "multi-user.target" ];
        serviceConfig.ExecStart = let
          bin = "${pkgs.selenium-server-standalone}/bin/selenium-server";
          cmd = [
            "${pkgs.xvfb_run}/bin/xvfb-run" "-s" "-screen 0 1024x768x24"
            bin "-port" "4444"
          ];
        in lib.concatMapStringsSep " " lib.escapeShellArg cmd;
        serviceConfig.User = "selenium";
      };
      # XXX: Figure out how to set the browser path for chromedriver, so we
      #      don't need this ugly workaround.
      system.activationScripts.chromium = ''
        mkdir -m 0755 -p /bin
        ln -sfn ${pkgs.chromium}/bin/chromium /bin/chromium
      '';
    };
  };
}
