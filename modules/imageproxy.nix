{ config, pkgs, lib, ... }:

let
  cfg = config.shabitica;

  go-camo = pkgs.buildGoPackage rec {
    name = "go-camo-${version}";
    version = "1.1.7";

    goPackagePath = "github.com/cactus/go-camo";

    src = pkgs.fetchFromGitHub {
      owner = "cactus";
      repo = "go-camo";
      rev = "v${version}";
      sha256 = "19fayml2138wk0lkbslwn1yg4bwcw88h09r1iwbr7xf7sjabad8k";
    };

    patches = [
      ../patches/imageproxy-socket-activation.patch
      ../patches/imageproxy-decode-session.patch
      ../patches/imageproxy-remove-metrics.patch
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
      { goPackagePath = "github.com/jessevdk/go-flags";
        rev = "v1.3.0";
        sha256 = "1jk2k2l10lwrn1r3nxdvbs0yz656830j4khzirw8p4ahs7c5zz36";
      }
      { goPackagePath = "github.com/coreos/go-systemd";
        rev = "v20";
        sha256 = "0jf705z62hy1230ryw9z3686g31v5yc16izcjzcasbawgv5c000v";
      }
    ];
  };

  # XXX: This is because NixOS 18.03 is using systemd version 237, which
  #      doesn't have the TemporaryFileSystem option.
  supportsTmpfs = lib.versionAtLeast config.systemd.package.version "238";

  sandboxPaths = pkgs.runCommand "shabitica-imageproxy-sandbox-paths" {
    closureInfo = pkgs.closureInfo { rootPaths = [ go-camo.bin ]; };
  } ''
    mkdir -p "$out/lib/systemd/system"
    serviceFile="$out/lib/systemd/system/shabitica-imageproxy.service"

    echo '[Service]' > "$serviceFile"

    while read storePath; do
      if [ ! -L "$storePath" ]; then
        echo "BindReadOnlyPaths=$storePath:$storePath:rbind"
      fi
    done < "$closureInfo/store-paths" >> "$serviceFile"
  '';

in {
  options.shabitica.imageProxy = {
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
      users.users.shabitica-imageproxy = {
        description = "Shabitica Image Proxy User";
        group = "shabitica-imageproxy";
      };

      users.groups.shabitica-imageproxy = {};

      systemd.sockets.shabitica-imageproxy = {
        description = "Shabitica Image Proxy Socket";
        requiredBy = [ "shabitica.service" ];

        socketConfig.ListenStream = "/run/shabitica-imageproxy.sock";
        socketConfig.SocketMode = "0660";
        socketConfig.SocketUser = "root";
        socketConfig.SocketGroup = config.services.nginx.group;
      };

      systemd.packages = [ sandboxPaths ];

      systemd.services.shabitica-imageproxy = {
        description = "Shabitica Image Proxy";
        serviceConfig = {
          ExecStart = "@${go-camo.bin}/bin/go-camo shabitica-imageproxy";
          User = "shabitica-imageproxy";
          Group = "shabitica-imageproxy";

          BindReadOnlyPaths = let
            mkEtcFile = etcfile: let
              hasFile = config.environment.etc ? ${etcfile};
              inherit (config.environment.etc.${etcfile}) source;
              sourceBind = "${source}:/etc/${etcfile}";
              fallback = "-/etc/${etcfile}";
            in if hasFile then sourceBind else fallback;
          in map mkEtcFile [ "resolv.conf" "ssl/certs/ca-certificates.crt" ];

          # Needed so that the proxy can validate sessions.
          EnvironmentFile = "/var/lib/shabitica/secrets.env";

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
          BindPaths = [ "/run/shabitica-imageproxy-chroot:/" ];
        });
      };
    })
    (lib.mkIf (cfg.imageProxy.enable && cfg.useNginx) {
      systemd.sockets.shabitica-imageproxy.wantedBy = [ "nginx.service" ];

      systemd.services.shabitica-imageproxy-cachedir = {
        description = "Create Shabitica Image Proxy Cache Dir";
        requiredBy = [ "nginx.service" ];
        before = [ "nginx.service" ];
        after = [ "local-fs.target" ];

        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;

        unitConfig.ConditionPathExists = "!/var/cache/shabitica-imageproxy";

        script = ''
          mkdir -p /var/cache/shabitica-imageproxy
          chmod 0710 /var/cache/shabitica-imageproxy
          chown root:nginx /var/cache/shabitica-imageproxy
        '';
      };

      services.nginx.commonHttpConfig = ''
        proxy_cache_path /var/cache/shabitica-imageproxy
          keys_zone=imageproxy:1m levels=1:2 inactive=30d max_size=1G;
      '';

      services.nginx.virtualHosts.${cfg.hostName}.locations."/imageproxy/" = {
        proxyPass = "http://unix:/run/shabitica-imageproxy.sock:/";
        extraConfig = ''
          proxy_cache imageproxy;
          proxy_cache_key $uri;
          proxy_cache_lock on;
          proxy_cache_use_stale error timeout invalid_header updating;
          proxy_cache_valid 200 30d;
          proxy_cache_valid any 0;
        '';
      };
    })
    (lib.mkIf (cfg.imageProxy.enable && !supportsTmpfs) {
      systemd.mounts = lib.singleton {
        description = "Tmpfs For Shabitica Image Proxy Chroot";

        bindsTo = [ "shabitica-imageproxy.service" ];
        requiredBy = [ "shabitica-imageproxy.service" ];
        before = [ "shabitica-imageproxy.service" ];
        after = [ "local-fs.target" ];

        what = "tmpfs";
        where = "/run/shabitica-imageproxy-chroot";
        type = "tmpfs";
        options = "nodev,noexec,nosuid";
      };
    })
  ];
}
