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

  habitipy = pkgs.python3Packages.buildPythonPackage rec {
    pname = "habitipy";
    version = "0.1.18";

    src = pkgs.fetchFromGitHub {
      owner = "ASMfreaK";
      repo = "habitipy";
      rev = "v${version}";
      sha256 = "1p79vvr9nlsg7d4md0r0pn43snmspjr5kgdjflm87slqwrv2zmkq";
    };

    propagatedBuildInputs = [
      pkgs.python3Packages.plumbum
      pkgs.python3Packages.requests
    ];

    doCheck = true;
    preCheck = "export HOME=\"$PWD\"";
    checkInputs = [
      pkgs.python3Packages.aiohttp pkgs.python3Packages.responses
      pkgs.python3Packages.hypothesis
    ];
  };

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

      newuser_foo = first.user.auth.local.register.post(
        username='foo',
        email='foo@example.org',
        password='snakeoil',
        confirmPassword='snakeoil',
      )

      foo = Habitipy({
        'url': 'http://localhost',
        'login': newuser_foo['id'],
        'password': newuser_foo['apiToken'],
      })

      newuser_bar = first.user.auth.local.register.post(
        username='bar',
        email='bar@example.org',
        password='snakeoil',
        confirmPassword='snakeoil',
      )

      bar = Habitipy({
        'url': 'http://localhost',
        'login': newuser_bar['id'],
        'password': newuser_bar['apiToken'],
      })

      foo.members.send_private_message.post(
        message='Hello Bar!',
        toUserId=newuser_bar['id']
      )

      reply = foo.tasks.user.post(
        type='habit',
        text='Test Habit',
        priority=2,
        up=True,
        down=True,
      )

      habit_id = reply['id']

      foo.tasks[habit_id].score['up'].post()
      foo.tasks[habit_id].score['up'].post()
      foo.tasks[habit_id].score['up'].post()
      foo.tasks[habit_id].score['down'].post()

      timewarp(86400)

      foo.tasks[habit_id].score['up'].post()

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

      reply = foo.challenges.post(
        group=party_id,
        name="Test a Challenge",
        shortName="testchallenge",
      )

      challenge_id = reply['id']

      reply = foo.tasks.challenge[challenge_id].post(
        type='habit',
        text='Test Challenge Habit',
        priority=2,
        up=True,
        down=True,
      )

      challenge_habit_id = reply['id']

      foo.groups[party_id].invite.post(uuids=[newuser_bar['id']])

      bar.groups[party_id].join.post()

      bar.challenges[challenge_id].join.post()

      challenge_bar_habit_id = bar.tasks.user.get(type='habits')[0]['id']

      bar.tasks[challenge_bar_habit_id].score['down'].post()

      timewarp(86100)

      bar.tasks[challenge_bar_habit_id].score['up'].post()

      # Date is 2018-01-03, warp to 2019-01-20 (382 days) because the birthday
      # gear will only be awarded when the user has logged in or has been
      # created after 2019-01-15.
      timewarp(33004800)

      newuser_birthday2019 = first.user.auth.local.register.post(
        username='birthday2019',
        email='birthday2019@example.org',
        password='snakeoil',
        confirmPassword='snakeoil',
      )

      loggedin = newuser_birthday2019['auth']['timestamps']['loggedin']
      assert loggedin.startswith('2019-01-20')

      # Same as with the birthday event, but for Pi-Day we need to warp to
      # 2019-02-20 (31 days since 2019-01-20) to be awarded the gear.
      timewarp(2678400)

      newuser_piday2019 = first.user.auth.local.register.post(
        username='piday2019',
        email='piday2019@example.org',
        password='snakeoil',
        confirmPassword='snakeoil',
      )

      loggedin = newuser_piday2019['auth']['timestamps']['loggedin']
      assert loggedin.startswith('2019-02-20')

      # The Half-Moon Glasses gear is only awarded for users logged in after
      # 2019-05-01, so let's warp to 2019-05-10.
      timewarp(6822000)

      newuser_halfmoon = first.user.auth.local.register.post(
        username='halfmoon',
        email='halfmoon@example.org',
        password='snakeoil',
        confirmPassword='snakeoil',
      )

      loggedin = newuser_halfmoon['auth']['timestamps']['loggedin']
      assert loggedin.startswith('2019-05-10')

      with open('spec.json', 'w') as fp:
        json.dump({
          'foo': {
            'apiUser': newuser_foo['id'],
            'apiToken': newuser_foo['apiToken'],
            'partyId': party_id,
            'habitId': habit_id,
            'challengeId': challenge_id,
            'challengeHabitId': challenge_habit_id,
          },
          'bar': {
            'apiUser': newuser_bar['id'],
            'apiToken': newuser_bar['apiToken'],
            'challengeUserHabitId': challenge_bar_habit_id,
          },
          'birthday2019': {
            'apiUser': newuser_birthday2019['id'],
            'apiToken': newuser_birthday2019['apiToken'],
          },
          'piday2019': {
            'apiUser': newuser_piday2019['id'],
            'apiToken': newuser_piday2019['apiToken'],
          },
          'halfmoon': {
            'apiUser': newuser_halfmoon['id'],
            'apiToken': newuser_halfmoon['apiToken'],
          }
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
    habitica.config.INVITE_ONLY = 0;
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
