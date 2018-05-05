{ lib, fetchurl
, libsass, libjpeg, optipng, gifsicle, pkgconfig, phantomjs2, systemd
, chromedriver, chromium

, habiticaConfig
}:

{
  main.habitica-markdown = drv: {
    preRebuild = (drv.preRebuild or "") + ''
      sed -i -e '
        s|https://s3.amazonaws.com/habitica-assets/cdn/emoji|${
          habiticaConfig.BASE_URL
        }/static/emojis|
      ' node_modules/habitica-markdown-emoji/index.js

      echo "checking whether anything refers to an S3 bucket..." >&2
      ! grep -r amazonaws .
    '';
  };

  main.node-sass = drv: let
    # Thin is needed to be backwards-compatible with NixOS stable.
    newerSass = libsass.overrideAttrs (drv: rec {
      name = "libsass-${version}";
      version = "3.5.3";
      patchPhase = "export LIBSASS_VERSION=${version}";
      src = fetchurl {
        url = "https://github.com/sass/libsass/archive/${version}.tar.gz";
        sha256 = "1lfdq2ahskf9yd0m71jlx3r4n6a0dhg4wxpnwbrvj2a23k7db7zi";
      };
    });
    isOutdated = lib.versionOlder libsass.version newerSass.version;
  in {
    LIBSASS_EXT = "auto";
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [ pkgconfig ];
    buildInputs = (drv.buildInputs or []) ++ [
      (if isOutdated then newerSass else libsass)
    ];
  };

  main.bcrypt = drv: {
    preRebuild = (drv.preRebuild or "") + ''
      sed -i -e 's/--fallback-to-build/--build-from-source/g' package.json
    '';
  };

  main.pageres = drv: {
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [ phantomjs2 ];
    # XXX: For PhantomJS 2 with NixOS 17.09:
    QT_QPA_PLATFORM = "offscreen";
  };

  main.sd-notify = drv: {
    buildInputs = (drv.buildInputs or []) ++ [ systemd ];
  };

  # XXX: This seems to be a caching issue in that process-nextick-args is
  # picked up from gulp-babel's node_modules instead of gulp's node_modules.
  main.gulp = drv: {
    preRebuild = (drv.preRebuild or "") + ''
      sed -i -e 's/pna\.nextTick/pna/g' \
        node_modules/readable-stream/lib/_stream_{writable,readable}.js
    '';
  };

  main.gulp-imagemin = drv: {
    preRebuild = (drv.preRebuild or "") + ''
      find -type f -exec sed -i -e '
        /new BinWrapper()/,/\.use(.*jpegtran/ {
          s!\.dest(.*)!.dest("'${libjpeg.bin}'/bin")!
        }

        /new BinWrapper()/,/\.use(.*optipng/ {
          s!\.dest(.*)!.dest("'${optipng}'/bin")!
        }

        /new BinWrapper()/,/\.use(.*gifsicle/ {
          s!\.dest(.*)!.dest("'${gifsicle}'/bin")!
        }
      ' {} +
    '';
  };

  dev.chromedriver = drv: let
    chromedriverBin = "${chromedriver}/bin/chromedriver";
    binJsString = "'${lib.escape ["\\" "'"] chromedriverBin}'";
  in {
    preRebuild = (drv.preRebuild or "") + ''
      # Remove the install script, so it won't try to download chromedriver
      sed -i -e '/"scripts": *{/,/},/d' package.json
      rm install.js

      # Hardcode the binary path to chromedriver
      sed -i -e '
        /^process\.env\.PATH *+=/d
        s!^\(exports\.path *= *\).*!\1'${lib.escapeShellArg binJsString}';!
      ' lib/chromedriver.js
    '';
  };

  dev.puppeteer = drv: let
    chromiumBin = "${chromium}/bin/chromium";
    chromiumJsString = "'${lib.escape ["\\" "'"] chromiumBin}'";
  in {
    PUPPETEER_SKIP_CHROMIUM_DOWNLOAD = true;

    preRebuild = (drv.preRebuild or "") + ''
      sed -i -e '/static \+executablePath().*{/ {
        :l; N; /^ *} *$/!bl
        c static executablePath() { return '${chromiumJsString}'; }
      }' lib/Launcher.js node6/lib/Launcher.js
    '';
  };
}
