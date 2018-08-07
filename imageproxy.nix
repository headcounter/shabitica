{ config, pkgs, lib, ... }:

let
  cfg = config.habitica;

  go-camo = pkgs.buildGoPackage rec {
    name = "go-camo-${version}";
    version = "1.1.2git2018-08-01";

    goPackagePath = "github.com/cactus/go-camo";

    src = pkgs.fetchFromGitHub {
      owner = "cactus";
      repo = "go-camo";
      rev = "7f62c2477c1072d7e7ef72962380ae8238a74e2c";
      sha256 = "0mx0scykr0dwshjqcc7ssqvlgdqn5rh554pfgxwk6wzh0cxv00qq";
    };

    patches = [
      ./patches/imageproxy-socket-activation.patch
      ./patches/imageproxy-decode-session.patch
    ];

    extraSrcs = let
      mkGoDep = { goPackagePath, rev, sha256, ... }@attrs: let
        matchRepoOwner = builtins.match "github\\.com/([^/]+)/([^/]+)";
        matchResult = matchRepoOwner goPackagePath;
      in if matchResult == null then attrs else {
        inherit goPackagePath;
        src = pkgs.fetchFromGitHub {
          repo = lib.last matchResult;
          owner = lib.head matchResult;
          inherit rev sha256;
        };
      };
    in map mkGoDep [
      { goPackagePath = "github.com/cactus/mlog";
        rev = "v1.0.1";
        sha256 = "05iz2a1ynbb9672clvykbvxs63wibyxlsl52wq4k7b53db0b09vh";
      }
      { goPackagePath = "github.com/cactus/tai64";
        rev = "v1.0.0";
        sha256 = "0x4dk5sw4ypiwd7gr5rjzp4avz0sagzs42r3dw852flh3lpvf4ir";
      }
      { goPackagePath = "github.com/davecgh/go-spew";
        rev = "v1.1.0";
        sha256 = "0d4jfmak5p6lb7n2r6yvf5p1zcw0l8j74kn55ghvr7zr7b7axm6c";
      }
      { goPackagePath = "github.com/jessevdk/go-flags";
        rev = "v1.3.0";
        sha256 = "1jk2k2l10lwrn1r3nxdvbs0yz656830j4khzirw8p4ahs7c5zz36";
      }
      { goPackagePath = "github.com/pelletier/go-toml";
        rev = "v1.1.0";
        sha256 = "1y5m9pngxhsfzcnxh8ma5nsllx74wn0jr47p2n6i3inrjqxr12xh";
      }
      { goPackagePath = "github.com/pmezard/go-difflib";
        rev = "v1.0.0";
        sha256 = "0c1cn55m4rypmscgf0rrb88pn58j3ysvc2d0432dp3c6fqg6cnzw";
      }
      { goPackagePath = "github.com/stretchr/testify";
        rev = "v1.2.1";
        sha256 = "01f80s0q64pw5drfgqwwk1wfwwkvd2lhbs56lhhkff4ni83k73fd";
      }
      { goPackagePath = "github.com/coreos/go-systemd";
        rev = "v17";
        sha256 = "1kzqrrzqspa5qm7kwslxl3m16lqzns23c24rv474ajzwmj3ixmx1";
      }
    ];
  };

  # XXX: This is because NixOS 18.03 is using systemd version 237, which
  #      doesn't have the TemporaryFileSystem option.
  supportsTmpfs = lib.versionAtLeast config.systemd.package.version "238";

  sandboxPaths = pkgs.runCommand "habitica-imageproxy-sandbox-paths" {
    closureInfo = pkgs.closureInfo { rootPaths = [ go-camo.bin ]; };
  } ''
    mkdir -p "$out/lib/systemd/system"
    serviceFile="$out/lib/systemd/system/habitica-imageproxy.service"

    echo '[Service]' > "$serviceFile"

    while read storePath; do
      if [ ! -L "$storePath" ]; then
        echo "BindReadOnlyPaths=$storePath:$storePath:rbind"
      fi
    done < "$closureInfo/store-paths" >> "$serviceFile"
  '';

in {
  options.habitica.imageProxy = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = cfg.useNginx;
      example = false;
      description = ''
        Whether to enable an image proxy used to locally cache all images in
        markdown. The default is <literal>true</literal> if any of the
        supported web server integrations is enabled, like for example when
        <option>useNginx</option> is enabled.
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.imageProxy.enable {
      users.users.habitica-imageproxy = {
        description = "Habitica Image Proxy User";
        group = "habitica-imageproxy";
      };

      users.groups.habitica-imageproxy = {};

      systemd.sockets.habitica-imageproxy = {
        description = "Habitica Image Proxy Socket";
        requiredBy = [ "habitica.service" ];

        socketConfig.ListenStream = "/run/habitica-imageproxy.sock";
        socketConfig.SocketMode = "0660";
        socketConfig.SocketUser = "root";
        socketConfig.SocketGroup = config.services.nginx.group;
      };

      systemd.packages = [ sandboxPaths ];

      systemd.services.habitica-imageproxy = {
        description = "Habitica Image Proxy";
        serviceConfig = {
          ExecStart = "@${go-camo.bin}/bin/go-camo habitica-imageproxy";
          User = "habitica-imageproxy";
          Group = "habitica-imageproxy";

          BindReadOnlyPaths = let
            mkEtcFile = etcfile: let
              hasFile = config.environment.etc ? ${etcfile};
              inherit (config.environment.etc.${etcfile}) source;
              sourceBind = "${source}:/etc/${etcfile}";
              fallback = "-/etc/${etcfile}";
            in if hasFile then sourceBind else fallback;
          in map mkEtcFile [ "resolv.conf" "ssl/certs/ca-certificates.crt" ];

          # Needed so that the proxy can validate sessions.
          EnvironmentFile = "/var/lib/habitica/secrets.env";

          RootDirectory = sandboxPaths;
          MountFlags = "private";
          RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
          PrivateDevices = true;

          SystemCallErrorNumber = "EPERM";
          SystemCallFilter = [
            "@basic-io" "@io-event" "@signal" "@file-system" "@process"
            "@network-io" "~listen" "~bind"
            "mprotect" "brk" "sched_getaffinity"
          ];

        } // (if supportsTmpfs then {
          TemporaryFileSystem = "/";
        } else {
          BindPaths = [ "/run/habitica-imageproxy-chroot:/" ];
        });
      };
    })
    (lib.mkIf (cfg.imageProxy.enable && cfg.useNginx) {
      systemd.sockets.habitica-imageproxy.wantedBy = [ "nginx.service" ];

      systemd.services.habitica-imageproxy-cachedir = {
        description = "Create Habitica Image Proxy Cache Dir";
        requiredBy = [ "nginx.service" ];
        before = [ "nginx.service" ];
        after = [ "local-fs.target" ];

        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;

        unitConfig.ConditionPathExists = "!/var/cache/habitica-imageproxy";

        script = ''
          mkdir -p /var/cache/habitica-imageproxy
          chmod 0710 /var/cache/habitica-imageproxy
          chown root:nginx /var/cache/habitica-imageproxy
        '';
      };

      services.nginx.commonHttpConfig = ''
        proxy_cache_path /var/cache/habitica-imageproxy
          keys_zone=imageproxy:1m levels=1:2 inactive=30d max_size=1G;
      '';

      services.nginx.virtualHosts.${cfg.hostName}.locations."/imageproxy/" = {
        proxyPass = "http://unix:/run/habitica-imageproxy.sock:/";
        extraConfig = ''
          proxy_cache imageproxy;
          proxy_cache_key $uri;
          proxy_cache_lock on;
          proxy_cache_use_stale error timeout invalid_header updating;
          proxy_cache_valid 200 30d;
          proxy_cache_valid any 5m;
        '';
      };
    })
    (lib.mkIf (cfg.imageProxy.enable && !supportsTmpfs) {
      systemd.mounts = lib.singleton {
        description = "Tmpfs For Habitica Image Proxy Chroot";

        bindsTo = [ "habitica-imageproxy.service" ];
        requiredBy = [ "habitica-imageproxy.service" ];
        before = [ "habitica-imageproxy.service" ];
        after = [ "local-fs.target" ];

        what = "tmpfs";
        where = "/run/habitica-imageproxy-chroot";
        type = "tmpfs";
        options = "nodev,noexec,nosuid";
      };
    })
  ];
}
