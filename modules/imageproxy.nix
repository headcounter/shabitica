{ config, lib, ... }:

let
  cfg = config.shabitica;
  pkgs = cfg.pinnedPkgs;

  go-camo = pkgs.callPackage ../pkgs/go-camo {};
  go-camo-bin = lib.getBin go-camo;

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
        isSystemUser = true;
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

      systemd.services.shabitica-imageproxy = {
        description = "Shabitica Image Proxy";

        confinement.enable = true;
        confinement.binSh = null;
        confinement.mode = "chroot-only";

        serviceConfig = {
          ExecStart = "@${go-camo-bin}/bin/go-camo shabitica-imageproxy";
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
          RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];

          SystemCallErrorNumber = "EPERM";
          SystemCallFilter = [
            "@basic-io" "@io-event" "@signal" "@file-system" "@process"
            "@network-io" "~listen" "~bind"
            "mprotect" "brk" "sched_getaffinity"
            # XXX: Apparently needed for some Go versions, figure out why!
            "uname" "pipe" "pipe2" "ioctl" "getrandom"
          ];
        };
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
          chmod 0770 /var/cache/shabitica-imageproxy
          chown root:nginx /var/cache/shabitica-imageproxy
        '';
      };

      systemd.services.nginx.serviceConfig.ReadWritePaths = [
        "/var/cache/shabitica-imageproxy"
      ];

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
  ];
}
