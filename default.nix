# When adding another module arg, don't forget to provide defaults.
{ config ? null, pkgs ? null, lib ? null, ... }:

let
  # We want our channel name to be as plain and simple as possible, so the
  # channel name should be just 'shabitica'. Unfortunately when using nix-env
  # with -qa, it tries to auto-call all the Nix expressions in nix-defexpr
  # which by default contains channels and thus also the 'shabitica' channel.
  #
  # Auto-calling our default.nix however doesn't work because we're not
  # delivering a set of packages but a NixOS module, so what we're doing here
  # is returning an empty attribute set whenever this module is auto-called so
  # that "nix-env -qa" will work again.
  #
  # NOTE: The check on 'lib' is intentional here, because if we'd reference
  #       'config' or 'pkgs', we'd short-circuit the fixpoint of the module
  #       system and thus run into an infinite recursion.
  isAutoCalled = lib == null;
  autoCalledOr = x: if isAutoCalled then {} else x;

  cfg = config.shabitica;

  mongodb = pkgs.mongodb.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [ patches/mongodb-systemd.patch ];
    buildInputs = (drv.buildInputs or []) ++ [ pkgs.systemd ];
    NIX_LDFLAGS = lib.toList (drv.NIX_LDFLAGS or []) ++ [ "-lsystemd" ];
  });

  hostIsFqdn = builtins.match ".+\\..+" cfg.hostName != null;
  isFqdnTxt = "builtins.match \".+\\\\..+\" config.shabitica.hostName != null";

  dbtools = pkgs.callPackage ./dbtools.nix {};

  docInfo = import ./docinfo.nix;

  migrations = import ./migrations.nix;
  latestDbVersion = lib.length migrations;

  # XXX: This is because NixOS 18.03 is using systemd version 237, which
  #      doesn't have the TemporaryFileSystem option.
  supportsTmpfs = lib.versionAtLeast config.systemd.package.version "238";

  # Results in a systemd service unit for the Shabitica server which only
  # contains BindReadOnlyPaths options. The rest of the service is defined
  # later in systemd.services.shabitica and the contents here are merged
  # accordingly.
  shabiticaSandboxPaths = pkgs.runCommand "shabitica-sandbox-paths" {
    closureInfo = pkgs.closureInfo {
      rootPaths = [ cfg.packages.server pkgs.coreutils ];
    };
  } ''
    mkdir -p "$out/lib/systemd/system"
    serviceFile="$out/lib/systemd/system/shabitica.service"

    echo '[Service]' > "$serviceFile"

    while read storePath; do
      if [ ! -L "$storePath" ]; then
        echo "BindReadOnlyPaths=$storePath:$storePath:rbind"
      fi
    done < "$closureInfo/store-paths" >> "$serviceFile"
  '';

in autoCalledOr {
  imports = [ ./imageproxy.nix ];

  options.shabitica = {
    hostName = lib.mkOption {
      type = lib.types.str;
      default = "localhost";
      example = "shabitica.example.org";
      description = "The host name to use for Shabitica.";
    };

    adminMailAddress = lib.mkOption {
      type = lib.types.str;
      default = "root@localhost";
      example = "shabitica-admin@example.org";
      description = "Email address of the administrator.";
    };

    backupInterval = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "daily";
      description = ''
        If this value is not <literal>null</literal>, create database backups
        on the interval specified. The format is described in <citerefentry>
          <refentrytitle>systemd.time</refentrytitle>
          <manvolnum>7</manvolnum>
        </citerefentry>, specifically the notes about
        <literal>OnCalendar</literal>.

        Otherwise if the value is <literal>null</literal>, you can still
        trigger a database backup manually by issuing <command>systemctl start
        shabitica-db-backup.service</command>.

        The database backups are stored in <option>backupDir</option>.
      '';
    };

    backupDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/backup/shabitica";
      description = let
        inherit (docInfo) archiveExampleFilename;
        exampleFile = "<replaceable>${archiveExampleFilename}</replaceable>";
        examplePath = "<replaceable>backupDir</replaceable>/${exampleFile}";
        cmd = docInfo.dbrestore + examplePath;
      in ''
        The path where backups are stored as MongoDB archives. To restore such
        a backup, the command <command>${cmd}</command> can be used.
      '';
    };

    senderMailAddress = lib.mkOption {
      type = lib.types.str;
      default = "shabitica@localhost";
      example = "shabitica@example.org";
      description = "The email address to use for sending notifications.";
    };

    baseURL = lib.mkOption {
      type = lib.types.str;
      default = let
        defaultScheme = if cfg.useSSL then "https" else "http";
      in "${defaultScheme}://${cfg.hostName}";
      defaultText = let
        schemeText = "if config.shabitica.useSSL then \"https\" else \"http\"";
        hostText = "config.shabitica.hostName";
      in lib.literalExample "\"\${${schemeText}}://\${${hostText}}\"";
      description = ''
        The base URL to use for serving web site content.
        If the default is used the URL scheme is dependent on whether
        <option>useSSL</option> is enabled or not.
      '';
    };

    insecureDB = lib.mkOption {
      type = lib.types.bool;
      default = false;
      internal = true;
      description = ''
        This is only used for testing and not recommended in production. It
        disables the networking namespace for MongoDB and binds to <systemitem
        class="ipaddress">127.0.0.1</systemitem> as well, so local users can
        read and write to the database at will.
      '';
    };

    staticPath = lib.mkOption {
      type = lib.types.path;
      default = cfg.packages.client;
      defaultText = lib.literalExample "shabitica.client";
      readOnly = true;
      description = "The path to the static assets of Shabitica.";
    };

    apiDocPath = lib.mkOption {
      type = lib.types.path;
      default = cfg.packages.apidoc;
      defaultText = lib.literalExample "shabitica.apidoc";
      readOnly = true;
      description = "The path to the API documentation.";
    };

    useSSL = lib.mkOption {
      type = lib.types.bool;
      default = hostIsFqdn;
      defaultText = lib.literalExample isFqdnTxt;
      description = ''
        Whether to allow HTTPS connections only. If <option>hostName</option>
        contains any dots the default is <literal>true</literal>, otherwise
        it's <literal>false</literal>.
      '';
    };

    useACME = lib.mkOption {
      type = lib.types.bool;
      default = cfg.useSSL;
      description = ''
        Whether to use ACME to get a certificate for the domain specified in
        <option>hostName</option>. Defaults to <literal>true</literal> if
        <option>useSSL</option> is enabled.
      '';
    };

    useNginx = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to create a virtual host for nginx.";
    };

    config = lib.mkOption {
      type = with lib.types; attrsOf (either int str);
      description = "Configuration options to pass to Shabitica.";
    };

    packages = lib.mkOption {
      type = lib.types.attrsOf lib.types.package;
      default = pkgs.callPackages ./shabitica.nix {
        shabiticaConfig = cfg.config;
      };
      defaultText = "pkgs.callPackages ./shabitica.nix {"
                  + " shabiticaConfig = config.shabitica.config; "
                  + "}";
      internal = true;
      readOnly = true;
      description = ''
        The internal components of Shabitica (server, client, migrator,
        etc...).
      '';
    };

    allowEmbedFrom = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      example = [ "https://example.com/" "https://*.example.net/" ];
      description = ''
        List of URLs which are allowed to embed Shabitica using one of the
        <tag class="element">frame</tag>, <tag class="element">iframe</tag>,
        <tag class="element">embed</tag> or <tag class="element">object</tag>
        tags.

        The syntax has to be in the form described in <link xlink:href="${
          "https://w3c.github.io/webappsec-csp/2/#match-source-expression"
        }"/>.
      '';
    };
  };

  config = lib.mkMerge [
    { shabitica.config = {
        ADMIN_EMAIL = cfg.adminMailAddress;
        NODE_ENV = "production";
        BASE_URL = cfg.baseURL;
        NODE_DB_URI = "mongodb://%2Frun%2Fshabitica%2Fdb.sock";
        PORT = "/run/shabitica.sock";
        MAILER_SOCKET = "/run/shabitica-mailer.sock";
      };

      users.users.shabitica-db = {
        description = "Shabitica Database User";
        group = "shabitica";
      };

      users.users.shabitica-mailer = {
        description = "Shabitica Mailer Daemon User";
        group = "shabitica-mailer";
      };

      users.users.shabitica = {
        description = "Shabitica User";
        group = "shabitica";
      };

      users.groups.shabitica = {};
      users.groups.shabitica-mailer = {};

      environment.systemPackages = [ dbtools ];

      systemd.packages = [ shabiticaSandboxPaths ];

      systemd.services.shabitica-statedir-init = {
        description = "Initialize Shabitica";
        wantedBy = [ "multi-user.target" ];
        after = [ "local-fs.target" ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        unitConfig.ConditionPathExists = "!/var/lib/shabitica";
        script = ''
          mkdir -p /var/lib/shabitica/db

          chmod 0710 /var/lib/shabitica
          chown root:shabitica /var/lib/shabitica

          echo ${toString latestDbVersion} > /var/lib/shabitica/db-version

          chmod 0700 /var/lib/shabitica/db
          chown shabitica-db:shabitica /var/lib/shabitica/db
        '';
      };

      systemd.services.shabitica-secrets-init = {
        description = "Initialize Secrets for Shabitica";
        wantedBy = [ "multi-user.target" ];
        after = [ "local-fs.target" "shabitica-statedir-init.service" ];
        unitConfig.ConditionPathExists = "!/var/lib/shabitica/secrets.env";
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        serviceConfig.UMask = "0077";
        serviceConfig.ExecStart = pkgs.writeScript "init-secrets.py" ''
          #!${pkgs.python3Packages.python.interpreter}
          import random, secrets
          secrets = {
            'SESSION_SECRET': secrets.token_hex(random.randint(50, 300)),
            'SESSION_SECRET_KEY': secrets.token_hex(32),
            'SESSION_SECRET_IV': secrets.token_hex(16)
          }
          lines = [key + '="' + val + '"\n' for key, val in secrets.items()]
          open('/var/lib/shabitica/secrets.env', 'w').write("".join(lines))
        '';
      };

      systemd.services.shabitica-init = {
        description = "Initialize Shabitica";
        wantedBy = [ "multi-user.target" ];
        after = [
          "local-fs.target"
          "shabitica-statedir-init.service"
          "shabitica-secrets-init.service"
        ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        unitConfig.ConditionPathExists = "!/run/shabitica";
        script = ''
          mkdir -p /run/shabitica
          chmod 0710 /run/shabitica
          chown shabitica-db:shabitica /run/shabitica
        '';
      };

      systemd.services.shabitica-db = {
        description = "Shabitica MongoDB Instance";
        wantedBy = [ "multi-user.target" ];
        after = [ "shabitica-init.service" ];

        serviceConfig.ExecStart = let
          mongoDbCfg = pkgs.writeText "mongodb.conf" (builtins.toJSON {
            net.bindIp = "/run/shabitica/db.sock"
                       + lib.optionalString cfg.insecureDB ",127.0.0.1";
            net.unixDomainSocket.filePermissions = "0660";
            storage.dbPath = "/var/lib/shabitica/db";
            processManagement.fork = false;
          });
        in "${mongodb}/bin/mongod --config ${mongoDbCfg}";

        serviceConfig.Type = "notify";
        serviceConfig.User = "shabitica-db";
        serviceConfig.Group = "shabitica";
        serviceConfig.PrivateTmp = true;
        serviceConfig.PrivateNetwork = !cfg.insecureDB;
      };

      systemd.services.shabitica-db-backup = {
        description = "Backup Shabitica Database";
        after = [ "shabitica-db.service" ];

        serviceConfig.Type = "oneshot";
        serviceConfig.PrivateTmp = true;
        serviceConfig.UMask = "0077";

        script = ''
          backupDir=${lib.escapeShellArg cfg.backupDir}
          mkdir -p "$backupDir"
          archiveFile="$(date +${docInfo.archiveDateFormat}).archive"
          ${dbtools}/bin/shabitica-db-dump --archive="$backupDir/$archiveFile"
        '';
      };

      systemd.services.shabitica-db-update = {
        description = "Apply Shabitica Database Updates";
        requiredBy = [ "shabitica.service" ];
        wantedBy = [ "multi-user.target" ];
        after = [
          "shabitica-db-backup.service" "shabitica-db.service"
          "shabitica-init.service"
        ];
        before = [ "shabitica.service" ];

        path = lib.singleton (pkgs.writeScriptBin "query-db-version" ''
          #!${pkgs.stdenv.shell} -e
          if [ ! -e /var/lib/shabitica/db-version ]; then
            current=0
          else
            declare -i current=$(< /var/lib/shabitica/db-version)
          fi
          latest=${toString latestDbVersion}
          case "$1" in
            needs-update)
              test $latest -ne $current;;
            new-versions)
              ${pkgs.coreutils}/bin/seq $(($current + 1)) $latest;;
          esac
        '');

        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        serviceConfig.EnvironmentFile = "/var/lib/shabitica/secrets.env";
        serviceConfig.User = "shabitica";
        serviceConfig.Group = "shabitica";
        serviceConfig.PrivateTmp = true;
        serviceConfig.PrivateNetwork = true;
        serviceConfig.PermissionsStartOnly = true;

        preStart = ''
          if query-db-version needs-update; then
            systemctl start shabitica-db-backup.service
          fi
        '';

        script = ''
          if query-db-version needs-update; then
            for ver in $(query-db-version new-versions); do
              ${cfg.packages.migrator}/bin/migrate "$ver"
            done
          fi
        '';

        postStart = ''
          echo ${toString latestDbVersion} > /var/lib/shabitica/db-version
        '';
      };

      systemd.sockets.shabitica-mailer = {
        description = "Socket For Shabitica Mailer Daemon";
        requiredBy = [ "shabitica.service" ];
        before = [ "shabitica.service" ];

        socketConfig.ListenStream = "/run/shabitica-mailer.sock";
        socketConfig.SocketMode = "0600";
        socketConfig.SocketUser = "shabitica";
        socketConfig.SocketGroup = "shabitica";
      };

      systemd.services.shabitica-mailer = {
        description = "Shabitica Mailer Daemon";

        after = [
          "exim.service" "nullmailer.service" "opensmtpd.service"
          "postfix.service"
        ];

        environment.MAIL_FROM = cfg.senderMailAddress;
        environment.SENDMAIL_PATH = "${config.security.wrapperDir}/sendmail";

        serviceConfig.ExecStart = let
          mailer = pkgs.haskellPackages.callPackage ./mailer {};
        in "${mailer}/bin/shabitica-mailer";
        serviceConfig.User = "shabitica-mailer";
        serviceConfig.Group = "shabitica-mailer";
        serviceConfig.PrivateNetwork = true;
      };

      systemd.sockets.shabitica = {
        description = "Shabitica Socket";
        wantedBy = [ "sockets.target" ];
        socketConfig.ListenStream = "/run/shabitica.sock";
        socketConfig.SocketMode = "0660";
        socketConfig.SocketUser = "root";
        socketConfig.SocketGroup = config.services.nginx.group;
      };

      systemd.services.shabitica = {
        description = "Shabitica";
        wantedBy = [ "multi-user.target" ];

        after = [
          "shabitica-init.service"
          "shabitica-db.service"
          "shabitica-mailer.service"
        ];

        serviceConfig = {
          Type = "notify";
          TimeoutStartSec = "10min";
          NotifyAccess = "all";
          ExecStart = "${cfg.packages.server}/bin/shabitica-server";
          User = "shabitica";
          Group = "shabitica";
          EnvironmentFile = "/var/lib/shabitica/secrets.env";

          # Everything related to restricting file system access.
          # More BindReadOnlyPaths options are brought in via the
          # shabiticaSandboxPaths derivation defined earlier.
          BindReadOnlyPaths = [
            "/run/shabitica/db.sock"
            "/run/shabitica-mailer.sock"
            "/run/systemd/notify"
          ];
          MountAPIVFS = true;
          MountFlags = "private";
          PrivateDevices = true;
          PrivateNetwork = true;
          PrivateTmp = true;
          PrivateUsers = true;
          ProtectControlGroups = true;
          ProtectKernelModules = true;
          ProtectKernelTunables = true;
          RootDirectory = shabiticaSandboxPaths;
        } // (if supportsTmpfs then {
          TemporaryFileSystem = "/";
        } else {
          BindPaths = [ "/run/shabitica-chroot:/" ];
        });
      };
    }
    (lib.mkIf (!supportsTmpfs) {
      systemd.mounts = lib.singleton {
        description = "Tmpfs For Shabitica Chroot";

        bindsTo = [ "shabitica.service" ];
        requiredBy = [ "shabitica.service" ];
        before = [ "shabitica.service" ];
        after = [ "local-fs.target" ];

        what = "tmpfs";
        where = "/run/shabitica-chroot";
        type = "tmpfs";
        options = "nodev,noexec,nosuid";
      };
    })
    (lib.mkIf cfg.useNginx {
      services.nginx.enable = lib.mkOverride 900 true;
      services.nginx.virtualHosts.${cfg.hostName} = {
        forceSSL = cfg.useSSL;
        enableACME = cfg.useACME;
        locations = let
          # This is ugly as hell and basically disables caching.
          # See https://github.com/NixOS/nixpkgs/issues/25485
          storeDirWorkaround = ''
            if_modified_since off;
            add_header Last-Modified "";
            etag off;
          '';
          commonHeaders = let
            frameAncestors = let
              allowed = lib.concatStringsSep " " cfg.allowEmbedFrom;
            in if cfg.allowEmbedFrom == [] then "'none'" else allowed;

            csp = lib.concatStringsSep "; " [
              "default-src ${cfg.baseURL} 'unsafe-eval' 'unsafe-inline'"
              "img-src ${cfg.baseURL} data:"
              "object-src 'none'"
              "frame-ancestors ${frameAncestors}"
            ];
          in ''
            add_header X-Content-Type-Options nosniff;
            add_header Referrer-Policy no-referrer;
            add_header Content-Security-Policy "${csp};";
            add_header X-XSS-Protection "1; mode=block";
          '';
        in {
          "/".root = cfg.staticPath;
          "/".index = "index.html";
          "/".tryFiles = "$uri $uri/ @backend";
          "/".extraConfig = storeDirWorkaround + commonHeaders;

          "@backend".proxyPass = "http://unix:/run/shabitica.sock:";
          "@backend".extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header   X-Real-IP        $remote_addr;
            proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
            proxy_set_header   X-NginX-Proxy    true;
            proxy_set_header   Host             $http_host;
            proxy_set_header   Upgrade          $http_upgrade;
            proxy_redirect     off;
            ${commonHeaders}
          '';

          "/apidoc".alias = cfg.apiDocPath;
          "/apidoc".index = "index.html";
          "/apidoc".extraConfig = commonHeaders;
        };
      };
    })
    (lib.mkIf (cfg.backupInterval != null) {
      systemd.timers.shabitica-db-backup = {
        description = "Backup Shabitica Database";
        wantedBy = [ "timers.target" ];
        timerConfig.OnCalendar = cfg.backupInterval;
        timerConfig.Persistent = true;
      };
    })
  ];
}
