{ lib, nodePackages, runCommand, pkgconfig, phantomjs2, jq
, openssl, libsass, fetchFromGitHub, libjpeg, optipng, gifsicle

, habiticaConfig
}:

nodePackages.habitica.overrideAttrs (drv: habiticaConfig // {
  configEnvJson = builtins.toJSON habiticaConfig;

  googleFonts = runCommand "google-fonts" {
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

  emojis = fetchFromGitHub {
    owner = "WebpageFX";
    repo = "emoji-cheat-sheet.com";
    rev = "c59bf0aad0a7238050c1a3896ecad650af227d59";
    sha256 = "1s696nsvndp4p697yiaq908s387gc0m2xmby7cab071xf2p8c4h7";
  };

  patches = [
    # Fix infinite redirection occuring whenever the BASE_URL contains a port
    # number.
    patches/redirect-fix-port.patch

    # Everybody gets a lifetime subscription.
    patches/subscriptions4all.patch

    # Apply the patch afterwards, so that we can keep its size small:
    patches/remove-external-services.patch

    # Don't allow anonymous users to register, we only want to invite people.
    patches/invite-only.patch

    # Poor mans sendmail implementation, because the official Habitica instance
    # uses MailChimp and it appears that the templates reside on their account.
    patches/sendmail.patch

    # Official Habitica has different mail addresses for different positions,
    # but for a private instance this is not really necessary. So let's use
    # ADMIN_EMAIL everywhere.
    patches/one-admin-mailaddr.patch

    # This thing takes way too much space, so let's remove it.
    patches/kill-footer.patch

    # Registration is allowed for the first user of the instance.
    patches/allow-register-first.patch

    # Don't try to charge for group plans.
    patches/free-group-plans.patch

    # We have subscriptions for all, so let's allow to change group leader in a
    # group that has a subscription.
    patches/always-permit-group-leader-change.patch

    # Don't restrict to use email domains such as habitica.com or habitrpg.com.
    patches/dont-restrict-email-domains.patch
  ];

  patchPhase = ":";

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
    "habitica\\.com"
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
    "website/README.md"
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

  # For PhantomJS 2 with NixOS 17.09:
  QT_QPA_PLATFORM = "offscreen";

  # Change all habitica.com URLs to use BASE_URL and all hardcoded email
  # addresses to use ADMIN_EMAIL:
  rewriteHabiticaURIs = let
    sedEscape = lib.escape ["\\" "&" "!"];
    mkHtmlMail = lib.replaceStrings ["@" "."] ["&commat;" "&period;"];

    escBaseURL = sedEscape habiticaConfig.BASE_URL;
    escSimpleMail = sedEscape habiticaConfig.ADMIN_EMAIL;
    escHtmlMail = sedEscape (mkHtmlMail habiticaConfig.ADMIN_EMAIL);

  in lib.concatStringsSep "; " [
    "s!https\\?://habitica\\.com!${escBaseURL}!g"
    "s![a-z]\\+@habitica\\.com!${escSimpleMail}!g"
    "s![a-z]\\+&commat;habitica&period;com!${escHtmlMail}!g"
  ];

  preRebuild = (drv.preRebuild or "") + ''
    # Remove package lock file, because we want to make sure we only use the
    # dependencies in package.json as we have patches altering the file.
    rm package-lock.json

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

    for patch in $patches; do
      echo "applying patch $patch" >&2
      patch -p1 < "$patch"
    done

    # See 'rewriteHabiticaURIs' attribute above.
    find . -path ./test -prune -o -path ./website/static -prune -o \
      -type f -exec sed -i -e "$rewriteHabiticaURIs" {} +

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
      | grep -v 'api-v3/groups.js:.*payments' \
      | grep -v 'libs/bannedSlurs.js://.*socialites' \
      || :)"

    if [ -n "$extServicesWithoutFalsePositives" ]; then
      echo "FATAL: We still have occurences of external services here:" >&2
      echo "$extServicesWithoutFalsePositives" >&2
      exit 1
    fi

    cp --no-preserve=mode -rt website/static "$googleFonts/fonts"
    cp --no-preserve=mode -rt website/static "$emojis/public/graphics/emojis"

    sed -i -e '
      s|https://s3.amazonaws.com/habitica-assets/cdn/emoji|${
        habiticaConfig.BASE_URL
      }/static/emojis|
    ' node_modules/habitica-markdown-emoji/index.js

    echo "checking whether emojis refer to S3 bucket..." >&2
    ! grep -r amazonaws node_modules/habitica-markdown-emoji

    sed -i -e '
      s!\.dest(.*)!.dest("'${libjpeg.bin}'/bin")!
    ' node_modules/jpegtran-bin/lib/index.js

    sed -i -e '
      s!\.dest(.*)!.dest("'${optipng}'/bin")!
    ' node_modules/optipng-bin/lib/index.js

    sed -i -e '
      s!\.dest(.*)!.dest("'${gifsicle}'/bin")!
    ' node_modules/gifsicle/lib/index.js

    # Merge config.json.example with $configEnvJson and create config.json:
    echo "$configEnvJson" | ${jq}/bin/jq -s '.[0] * .[1]' \
      config.json.example - > config.json

    # Don't build bundled libsass:
    export LIBSASS_EXT=auto

    # Somehow node2nix didn't pick up the pinned dependency of async-done to
    # process-nextick-args version 1.0.7:
    sed -i -e 's/\<tick(/tick.nextTick(/' node_modules/async-done/index.js
  '';
  buildInputs = (drv.buildInputs or []) ++ [
    openssl
    (libsass.overrideAttrs (drv: rec {
      name = "libsass-${version}";
      version = assert lib.versionOlder drv.version "3.5.0"; "3.5.0";
      patchPhase = "export LIBSASS_VERSION=${version}";
      src = fetchFromGitHub {
        owner = "sass";
        repo = "libsass";
        rev = version;
        sha256 = "06ch6af7ivmx1f5l3z3dx3iqdiiwis1vcc1j4wdm10f6mlq11yxx";
      };
    }))
  ];
  nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [
    nodePackages.node-pre-gyp
    nodePackages.gulp-cli
    nodePackages.mocha
    pkgconfig
    phantomjs2
  ];
})
