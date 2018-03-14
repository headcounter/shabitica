{ config, pkgs, lib, ... }:

let
  habitica = let
    nodePackages = import ./generated { inherit pkgs; };
    getPackage = name: let
      attrs = lib.attrNames nodePackages;
      attrName = lib.head (lib.filter (lib.hasPrefix name) attrs);
    in nodePackages.${attrName};
  in (getPackage "habitica").overrideAttrs (drv: config.habitica.config // {
    configEnvJson = builtins.toJSON config.habitica.config;

    googleFonts = pkgs.runCommand "google-fonts" {
      name = "google-fonts";
      outputHashAlgo = "sha256";
      outputHash = "09sk5s4abrrwpxvzajc0lyq8i15p77vjr0brh8jq0pi2s2fnadi9";
      outputHashMode = "recursive";
      nativeBuildInputs = [ nodePackages.google-fonts-offline ];
      inherit (drv) src;
    } ''
      fontURL="$(sed -n \
        -e '/fonts\.googleapis/s/^.*href="\([^"]\+\)".*$/\1/p' \
        "$src/website/client/index.html")"
      mkdir "$out"
      ( cd "$out"
        goofoffline outCss=fonts.css "$fontURL"
      )
    '';

    # We don't want to have anything in the code referencing any of these
    # words/regexes:
    disallowedCanaries = lib.concatStringsSep "\\|" [
      "\\<apn"
      "\\<buygemsmodal\\>"
      "\\<payments\\>"
      "\\<sendgemsmodal\\>"
      "amazon"
      "amz"
      "analytics"
      "apple"
      "facebook"
      "fcm"
      "gcm"
      "google"
      "hellojs"
      "instagram"
      "itunes"
      "kafka"
      "loggly"
      "paypal"
      "play.*api"
      "play.*store"
      "pushnotif"
      "slack"
      "social"
      "stripe[^d]"
      "transifex"
      "twitter"
    ];

    excludedCanaryPaths = let
      mkExclude = path: "-path ${lib.escapeShellArg "./${path}"} -prune";
    in lib.concatMapStringsSep " -o " mkExclude [
      "Dockerfile-Production"
      "database_reports"
      "gulp"
      "migrations"
      "node_modules"
      "package-lock.json"
      "package.json"
      "test"
      "website/client/assets"
      "website/client/components/settings/api.vue"
      "website/client/components/static/privacy.vue"
      "website/client/components/static/terms.vue"
      "website/common/locales"
      "website/raw_sprites"
      "website/server/libs/bannedWords.js"
      "website/static/audio"
      "website/static/merch"
      "website/static/presskit"
    ];

    preRebuild = (drv.preRebuild or "") + ''
      patch -p1 < ${patches/redirect-fix-port.patch}
      patch -p1 < ${patches/subscriptions4all.patch}

      # Kill off files we do not want to have, as they redirect to external
      # services:
      ${lib.concatMapStrings (path: ''
        rm ${lib.escapeShellArg path}
      '') [
        "scripts/paypalBillingSetup.js"
        "website/client/components/payments/amazonModal.vue"
        "website/client/components/payments/buyGemsModal.vue"
        "website/client/components/payments/sendGemsModal.vue"
        "website/client/libs/analytics.js"
        "website/client/libs/logging.js"
        "website/client/libs/payments.js"
        "website/client/mixins/payments.js"
        "website/server/controllers/api-v3/iap.js"
        "website/server/controllers/top-level/payments/amazon.js"
        "website/server/controllers/top-level/payments/iap.js"
        "website/server/controllers/top-level/payments/paypal.js"
        "website/server/controllers/top-level/payments/stripe.js"
        "website/server/libs/amazonPayments.js"
        "website/server/libs/analyticsService.js"
        "website/server/libs/applePayments.js"
        "website/server/libs/aws.js"
        "website/server/libs/googlePayments.js"
        "website/server/libs/inAppPurchases.js"
        "website/server/libs/paypalPayments.js"
        "website/server/libs/pushNotifications.js"
        "website/server/libs/pusher.js"
        "website/server/libs/slack.js"
        "website/server/libs/stripePayments.js"
        "website/server/middlewares/analytics.js"
      ]}

      # Apply the patch afterwards, so that we can keep its size small:
      patch -p1 < ${patches/remove-external-services.patch}

      # Don't allow anonymous users to register, we only want to invite people.
      patch -p1 < ${patches/invite-only.patch}

      # Poor mans sendmail implementation, because the official Habitica
      # instance uses MailChimp and it appears that the templates reside on
      # their account.
      patch -p1 < ${patches/sendmail.patch}

      # Official Habitica has different mail addresses for different positions,
      # but for a private instance this is not really necessary. So let's use
      # ADMIN_EMAIL everywhere.
      patch -p1 < ${patches/one-admin-mailaddr.patch}

      # This thing takes way too much space, so let's remove it.
      patch -p1 < ${patches/kill-footer.patch}

      # Registration is allowed for the first user of the instance.
      patch -p1 < ${patches/allow-register-first.patch}

      echo "checking whether we have external services in the code..." >&2
      extServices="$(
        eval find . $excludedCanaryPaths -o -type f \
          -exec grep -Hi '"$disallowedCanaries"' {} + || :
      )"

      extServicesWithoutFalsePositives="$(echo "$extServices" \
        | grep -v 'user/schema\.js:.*\(facebook\|google\)' \
        | grep -v 'user/methods\.js:.*\<payments\>' \
        | grep -v 'models/group\.js:.*\<payments\>' \
        | grep -v 'settings/api\.vue:.*chrome\.google\.com/webstore' \
        | grep -v 'top-level/pages\.js:// All' \
        | grep -v 'api-v3/tasks\.js: *//.*pushNotif' \
        | grep -v 'user/schema\.js: *pushNotifications:' \
        | grep -v 'user/methods\.js:schema\.statics\.pushNotification' \
        || :)"

      if [ -n "$extServicesWithoutFalsePositives" ]; then
        echo "FATAL: We still have occurences of external services here:" >&2
        echo "$extServicesWithoutFalsePositives" >&2
        exit 1
      fi

      cp --no-preserve=mode -rt website/static "$googleFonts/fonts"

      sed -i -e '
        s!\.dest(.*)!.dest("'${pkgs.libjpeg.bin}'/bin")!
      ' node_modules/jpegtran-bin/lib/index.js

      sed -i -e '
        s!\.dest(.*)!.dest("'${pkgs.optipng}'/bin")!
      ' node_modules/optipng-bin/lib/index.js

      sed -i -e '
        s!\.dest(.*)!.dest("'${pkgs.gifsicle}'/bin")!
      ' node_modules/gifsicle/lib/index.js

      # Merge config.json.example with $configEnvJson and create config.json:
      echo "$configEnvJson" | ${pkgs.jq}/bin/jq -s '.[0] * .[1]' \
        config.json.example - > config.json

      # Don't build bundled libsass:
      export LIBSASS_EXT=auto

      # Somehow node2nix didn't pick up the pinned dependency of async-done to
      # process-nextick-args version 1.0.7:
      sed -i -e 's/\<tick(/tick.nextTick(/' node_modules/async-done/index.js
    '';
    buildInputs = (drv.buildInputs or []) ++ [
      pkgs.openssl
      (pkgs.libsass.overrideAttrs (drv: rec {
        name = "libsass-${version}";
        version = assert lib.versionOlder drv.version "3.5.0"; "3.5.0";
        patchPhase = "export LIBSASS_VERSION=${version}";
        src = pkgs.fetchFromGitHub {
          owner = "sass";
          repo = "libsass";
          rev = version;
          sha256 = "06ch6af7ivmx1f5l3z3dx3iqdiiwis1vcc1j4wdm10f6mlq11yxx";
        };
      }))
    ];
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [
      nodePackages.node-pre-gyp
      (getPackage "gulp")
      nodePackages.mocha
      pkgs.pkgconfig
      pkgs.phantomjs2
    ];
  });

  basePath = "${habitica}/lib/node_modules/habitica";
  basePathText = "\${habitica}/lib/node_modules/habitica";

in {
  options.habitica = {
    baseURL = lib.mkOption {
      type = lib.types.str;
      default = "https://habitica.headcounter.org";
      description = ''
        The base URL to use for serving web site content.
      '';
    };

    staticPath = lib.mkOption {
      type = lib.types.path;
      default = "${basePath}/dist-client/static";
      defaultText = "${basePathText}/dist-client/static";
      readOnly = true;
      description = ''
        The path to the static assets of Habitica.
      '';
    };

    apiDocPath = lib.mkOption {
      type = lib.types.path;
      default = "${basePath}/apidoc_build";
      defaultText = "${basePathText}/apidoc_build";
      readOnly = true;
      description = ''
        The path to the API documentation.
      '';
    };

    config = lib.mkOption {
      type = with lib.types; attrsOf (either int str);
      description = ''
        Configuration options to pass to Habitica.
      '';
    };
  };

  config = {
    habitica.config = {
      ADMIN_EMAIL = "aszlig@nix.build";
      NODE_ENV = "production";
      BASE_URL = config.habitica.baseURL;
      NODE_DB_URI = "mongodb:///run/habitica/db.sock";
      PORT = "/run/habitica.sock";
      SENDMAIL_PATH = "${config.security.wrapperDir}/sendmail";
      MAIL_FROM = "no-reply@headcounter.org";
    };

    users.users.habitica-db = {
      description = "Habitica Database User";
      group = "habitica";
    };

    users.users.habitica = {
      description = "Habitica User";
      group = "habitica";
    };

    users.groups.habitica = {};

    systemd.services.habitica-statedir-init = {
      description = "Initialize Habitica";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      unitConfig.ConditionPathExists = "!/var/lib/habitica";
      script = ''
        mkdir -p /var/lib/habitica/db /var/lib/habitica/data

        chmod 0710 /var/lib/habitica
        chown root:habitica /var/lib/habitica

        chmod 0700 /var/lib/habitica/db
        chown habitica-db:habitica /var/lib/habitica/db

        chmod 0700 /var/lib/habitica/data
        chown habitica:habitica /var/lib/habitica/data
      '';
    };

    systemd.services.habitica-init = {
      description = "Initialize Habitica";
      wantedBy = [ "multi-user.target" ];
      after = [ "local-fs.target" "habitica-statedir-init.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      unitConfig.ConditionPathExists = "!/run/habitica";
      script = ''
        mkdir -p /run/habitica
        chmod 0710 /run/habitica
        chown habitica-db:habitica /run/habitica
      '';
    };

    systemd.services.habitica-db = {
      description = "Habitica MongoDB Instance";
      wantedBy = [ "multi-user.target" ];
      after = [ "habitica-init.service" ];

      serviceConfig.ExecStart = let
        mongoDbCfg = pkgs.writeText "mongodb.conf" (builtins.toJSON {
          net.bindIp = "/run/habitica/db.sock";
          net.unixDomainSocket.filePermissions = "0770";
          storage.dbPath = "/var/lib/habitica/db";
          processManagement.fork = false;
        });
      in "${pkgs.mongodb}/bin/mongod --config ${mongoDbCfg}";

      serviceConfig.User = "habitica-db";
      serviceConfig.Group = "habitica";
      serviceConfig.PrivateTmp = true;
      serviceConfig.PrivateNetwork = true;
    };

    systemd.sockets.habitica = {
      description = "Habitica Socket";
      wantedBy = [ "sockets.target" ];
      socketConfig.ListenStream = "/run/habitica.sock";
      socketConfig.SocketMode = "0660";
      socketConfig.SocketUser = "root";
      socketConfig.SocketGroup = config.services.nginx.group;
    };

    systemd.services.habitica = {
      description = "Habitica";
      after = [ "habitica-init.service" "habitica-db.service" ];

      environment = config.habitica.config;

      serviceConfig.ExecStart = let
        websitePath = "${habitica}/lib/node_modules/habitica/website";
      in lib.concatMapStringsSep " " lib.escapeShellArg [
        "@${pkgs.nodejs-6_x}/bin/node" "habitica-server"
        "${websitePath}/transpiled-babel/index.js"
      ];

      serviceConfig.User = "habitica";
      serviceConfig.Group = "habitica";
      serviceConfig.PrivateTmp = true;
      serviceConfig.PrivateNetwork = true;
      serviceConfig.WorkingDirectory = "${habitica}/lib/node_modules/habitica";
    };
  };
}
