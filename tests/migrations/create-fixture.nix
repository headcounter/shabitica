{ system ? builtins.currentSystem }:

let
  # Some revision that's known to evaluate with version 4.38.0.
  nixpkgsRev = "c505e5777143a35828494e2c1e9744940b224a5d";
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "0jhh1v9jychwvvixcj3r13zqx987p464k9lxqa1kynjayaizkla1";
  };

  # Shortly before the upgrade to version 4.40.0, which is the first
  # version in our standalone fork that needs migration.
  habiticaRev = "3686bb79a9e988207ef0d98f72f904b8f9516e68";
  habitica = fetchTarball {
    url = "https://github.com/headcounter/shabitica/archive/"
        + "${habiticaRev}.tar.gz";
    sha256 = "1dq8babnj5dfkjgnfirw989frkss7z4vb7pjxkk1xw96g4b72pas";
  };

  inherit (import "${nixpkgs}/nixos/lib/testing.nix" {
    inherit system;
  }) pkgs runInMachine;
  inherit (pkgs) lib;

  habitipy = pkgs.python3Packages.callPackage ./habitipy.nix {};

in runInMachine {
  drv = pkgs.runCommand "habitica-migration-fixture" {
    outputs = [ "out" "spec" ];

    nativeBuildInputs = [
      habitipy pkgs.python3Packages.python pkgs.mongodb-tools
    ];

    populate = ''
      import json
      from habitipy import Habitipy

      first = Habitipy({
        'url': 'http://localhost',
        'login': "",
        'password': "",
      })

      newuser = first.user.auth.local.register.post(
        username='foo',
        email='foo@example.org',
        password='snakeoil',
        confirmPassword='snakeoil',
      )

      foo = Habitipy({
        'url': 'http://localhost',
        'login': newuser['id'],
        'password': newuser['apiToken'],
      })

      reply = foo.groups.post(
        name='Testparty',
        type='party',
        privacy='private',
      )

      party_id = reply['id']

      foo.groups[party_id].chat.post(
        message="Hello World!",
        previousMsg="",
      )

      with open('spec.json', 'w') as fp:
        json.dump({
          'foo': {
            'apiUser': newuser['id'],
            'apiToken': newuser['apiToken'],
            'partyId': party_id,
          },
        }, fp)
    '';

  } ''
    echo "$populate" | python3
    mongodump
    find dump/admin -mindepth 1 -maxdepth 1 -exec mv -t "$out" {} +
    rmdir -p dump/admin # To make sure it's empty
    mv spec.json "$spec"
  '';

  machine = {
    imports = [ habitica ];
    networking.firewall.enable = false;
    habitica.insecureDB = true;
    virtualisation.diskSize = 16384;
    virtualisation.memorySize = 1024;
  };
}
