let
  configuration = { config, pkgs, lib, ... }: let
    sshKeyPair = pkgs.runCommand "ssh-keypair" {
      buildInputs = [ pkgs.openssh ];
    } ''
      mkdir "$out"
      ssh-keygen -t ed25519 -f "$out/key" -N "" -C "$keyComment"
    '';

    vuizvui = (import <nixpkgs> {}).fetchFromGitHub {
      owner = "openlab-aux";
      repo = "vuizvui";
      rev = "e55694bfb31d310204eee1b6586a10ac38e8d57f";
      sha256 = "03h65rhrykfsy8b8adqw0amz80v5xs1cqizhy9zxfmzvxa575fvz";
    };

    zshModule = "${vuizvui}/modules/user/aszlig/programs/zsh";
    vuizvuiPkgs = (import "${vuizvui}/pkgs" { inherit pkgs; }).vuizvui;

    mailcap = let
      mkEntry = attrs: let
        optAttrs = removeAttrs attrs [ "command" ];
        mkOpt = key: val:
          if val == true then key
          else if val == false then null
          else "${key}=${val}";
        opts = lib.remove null (lib.mapAttrsToList mkOpt optAttrs);
      in lib.concatStringsSep "; " ([ attrs.type attrs.command ] ++ opts);
    in pkgs.writeText "mailcap" (lib.concatMapStringsSep "\n" mkEntry [
      { type = "text/html";
        command = "${pkgs.w3m}/bin/w3m -T text/html '%s'";
        needsterminal = true;
        description = "HTML Text";
        nametemplate = "%s.html";
      }
      { type = "text/html";
        command = "${pkgs.w3m}/bin/w3m -T text/html '%s'";
        copiousoutput = true;
        description = "HTML Text";
        nametemplate = "%s.html";
      }
    ]);

  in {
    imports = [ ./.. zshModule ];

    shabitica.hostName = "localhost";
    shabitica.baseURL = "http://localhost:3000";
    shabitica.config.ENABLE_CONSOLE_LOGS_IN_PROD = "true";
    shabitica.insecureDB = true;

    environment.systemPackages = [
      pkgs.htop pkgs.mongodb-tools
      (pkgs.mutt.overrideAttrs (attrs: {
        configureFlags = (attrs.configureFlags or []) ++ [
          "--with-domain=${config.networking.hostName}"
        ];
        postInstall = (attrs.postInstall or "") + ''
          cat >> "$out/etc/Muttrc" <<MUTTRC
          alternative_order text/plain text/enriched text/html
          auto_view text/html
          bind attach <return> view-mailcap
          set ascii_chars=yes
          set folder = \$MAIL
          set mailcap_path = ${mailcap}
          set sort=threads
          MUTTRC
        '';
      }))
      vuizvuiPkgs.aszlig.vim
    ];

    vuizvui.user.aszlig.programs.zsh.enable = true;
    vuizvui.user.aszlig.programs.zsh.machineColor = "yellow";
    users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";
    time.timeZone = "Europe/Berlin";

    networking.hostName = "shabitica-dev";
    networking.firewall.enable = false;

    services.postfix.enable = true;
    services.postfix.virtual = "/.*/ root\n";
    services.postfix.virtualMapType = "regexp";
    services.postfix.config = {
      inet_interfaces = "127.0.0.1";
      virtual_alias_domains = "";
    };

    services.openssh.enable = true;

    services.journald.rateLimitInterval = "0";

    system.build.wrapped-vm = let
      sleep = lib.escapeShellArg "${pkgs.coreutils}/bin/sleep";
      nc = lib.escapeShellArg "${pkgs.netcat-openbsd}/bin/nc";
      ssh = lib.escapeShellArg "${pkgs.openssh}/bin/ssh";

      connect = lib.concatMapStringsSep " " lib.escapeShellArg [
        "${pkgs.openssh}/bin/ssh"
        "-i" "${sshKeyPair}/key"
        "-o" "UserKnownHostsFile=/dev/null"
        "-o" "GlobalKnownHostsFile=/dev/null"
        "-o" "StrictHostKeyChecking=no"
        "-o" "ConnectionAttempts=10"
        "-p" "3022"
        "root@localhost"
      ];

    in pkgs.writeScript "run-vm" ''
      #!${pkgs.stdenv.shell}

      if [ "$1" = '--connect' ]; then
        shift
        exec ${connect} "$@"
        exit 1
      elif [ "$1" = '--switch' ]; then
        newsys=${lib.escapeShellArg config.system.build.toplevel}
        exec ${connect} "$newsys/bin/switch-to-configuration" test
        exit 1
      elif [ -n "$1" ]; then
        echo "Usage: $0 [--connect|--switch]" >&2
        exit 1
      fi

      if ${nc} -z 127.0.0.1 3022; then
        echo "VM already running, use '--connect' to connect to it." >&2
        exit 1
      fi

      kill_everything() {
        retry=0
        while kill -0 $(jobs -p); do
          if [ $retry -ge 15 ]; then
            kill -9 $(jobs -p)
          else
            kill $(jobs -p)
          fi
          retry=$(($retry + 1))
          ${sleep} 0.1
        done 2> /dev/null || :
      }

      waitport_ssh() {
        while ! ${nc} -z 127.0.0.1 "$1"; do ${sleep} 0.1; done
        while ! ${nc} -w1 127.0.0.1 "$1" < /dev/null | grep -q -m1 '^SSH-'; do
          ${sleep} 0.1
        done
      }

      trap kill_everything EXIT

      set -e

      ${nc} -u -l 127.0.0.1 3332 &
      ncpid=$!

      ${lib.escapeShellArg config.system.build.vm}/bin/run-*-vm \
        -monitor tcp:127.0.0.1:3331,server,nowait \
        -serial udp:127.0.0.1:3332 \
        "$@" &
      vmpid=$!

      waitport_ssh 3022

      set +e
      ${connect}
      retval=$?
      set -e

      echo system_powerdown | ${nc} 127.0.0.1 3331 > /dev/null
      wait $vmpid || :
      exit $retval
    '';

    systemd.services."serial-getty@ttyS0".enable = false;
    systemd.services."serial-getty@hvc0".enable = false;

    environment.etc."ssh/authorized_keys.d/root" = lib.mkForce {
      mode = "0444";
      source = "${sshKeyPair}/key.pub";
    };

    virtualisation.diskSize = 16384;
    virtualisation.memorySize = 1024;
    virtualisation.graphics = false;

    virtualisation.qemu.networkingOptions = let
      devOpts = lib.concatStringsSep "," [
        "hostfwd=tcp:127.0.0.1:3000-:80"
        "hostfwd=tcp:127.0.0.1:3022-:22"
      ];
    in [
      "-device virtio-net-pci,netdev=vlan0"
      "-netdev user,id=vlan0,${devOpts}\${QEMU_NET_OPTS:+,$QEMU_NET_OPTS}"
    ];

    virtualisation.qemu.options = [ "-device virtio-rng-pci" ];
  };

in (import <nixpkgs/nixos/lib/eval-config.nix> {
  system = builtins.currentSystem;
  modules = [
    configuration <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
  ];
}).config.system.build.wrapped-vm
