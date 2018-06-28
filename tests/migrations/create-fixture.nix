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
      import socket
      import struct
      from habitipy import Habitipy

      def timewarp(secs):
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect('/run/timewarp.sock')
        sock.sendall(struct.pack('Q', secs))
        if sock.recv(1) == bytes([0]):
          raise Exception("Unable to forward time by {} seconds.".format(secs))
        sock.close()

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

    boot.postBootCommands = ''
      ${pkgs.coreutils}/bin/date -s '2018-01-01 12:00:00'
    '';

    systemd.sockets.timewarp = {
      description = "Time Warp Socket";
      wantedBy = [ "sockets.target" ];
      socketConfig.ListenStream = "/run/timewarp.sock";
    };

    systemd.services."timewarp" = {
      description = "Time Warp Service";
      serviceConfig.Type = "oneshot";
      serviceConfig.ExecStart = let
        script = pkgs.writeText "timewarp.py" ''
          import socket
          import struct
          import subprocess
          import sys
          import time

          sock = socket.fromfd(3, socket.AF_UNIX, socket.SOCK_STREAM)

          def warp_by(secs):
            previous = time.time()
            cmd = ["date", "-s", "now + {} seconds".format(secs)]
            subprocess.check_call(cmd, stdout=subprocess.DEVNULL)
            now = time.time()
            assert previous + secs <= now

          try:
            while True:
              conn = sock.accept()[0]
              reply = conn.recv(8)
              secs = struct.unpack('Q', reply)[0]
              sys.stderr.write("Warping time by {} seconds.".format(secs))
              try:
                warp_by(secs)
                conn.send(bytes([1]))
              except:
                conn.send(bytes([0]))
                raise
              finally:
                conn.close()
          finally:
            sock.close()
        '';
      in "${pkgs.python3.interpreter} ${script}";
    };
  };
}
