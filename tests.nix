{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) lib;

  habitica = pkgs.callPackages ./habitica.nix {
    habiticaConfig = rec {
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

  mkTest = name: { target, useDB ? false }: habitica.mkCommonBuild {
    name = "test-${name}";
    gulpTarget = "test:${target}";

    nativeBuildInputs = lib.attrValues habitica.nodePackages.dev
                     ++ lib.singleton pkgs.mongodb;

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

    installPhase = "touch \"$out\"";
  };

  runTests = cat: lib.mapAttrs (name: mkTest "${cat}-${name}");

in lib.mapAttrs runTests {
  basic = {
    sanity.target = "sanity";
    content.target = "content";
    common.target = "common";
  };

  api = {
    unit.target = "api-v3:unit";
    unit.useDB = true;

    integration.target = "api-v3:integration";
    integration.useDB = true;
  };
}
