{ super, lib, fetchurl, substituteAll, runCommand
, libsass, libjpeg, optipng, gifsicle, pkgconfig, phantomjs2, systemd
, chromedriver, chromium

, shabiticaConfig
}:

{
  main.habitica-markdown = drv: {
    preRebuild = (drv.preRebuild or "") + ''
      patch -p1 < ${substituteAll {
        src = ./patches/habitica-markdown-imageproxy.patch;
        proxyUrl = "${shabiticaConfig.BASE_URL}/imageproxy/";
        baseUrl = shabiticaConfig.BASE_URL;
      }}

      sed -i -e '
        s|https://s3.amazonaws.com/habitica-assets/cdn/emoji|${
          shabiticaConfig.BASE_URL
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

  main.pageres = drv: {
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [ phantomjs2 ];
    # XXX: For PhantomJS 2 with NixOS 18.03:
    QT_QPA_PLATFORM = "offscreen";
  };

  main.sd-notify = drv: {
    buildInputs = (drv.buildInputs or []) ++ [ systemd ];
  };

  # We don't want to load fonts from Google, but instead ship it ourselves.
  main.apidoc = drv: {
    # FIXME: This is not deterministic, find a better way...
    googleFonts = runCommand "google-fonts-apidoc" {
      outputHashAlgo = "sha256";
      outputHash = "1kkv6nrswzxcfdyyx46r6238ss89xj5nayji8jgr590lca1bi1sy";
      outputHashMode = "recursive";
      nativeBuildInputs = [ super.extra.google-fonts-offline ];

      fontURL = "https://fonts.googleapis.com/css?family="
              + "Source+Code+Pro%7CSource+Sans+Pro:n4,n6,n7";
    } ''
      mkdir "$out"
      cd "$out"
      goofoffline outCss=fonts.css "$fontURL"
    '';

    preRebuild = (drv.preRebuild or "") + ''
      rm template/vendor/webfontloader.js
      rm template/vendor/prettify/run_prettify.js

      cp -t template/fonts "$googleFonts/fonts/"*
      sed -i -e '/<\/head>/i \
        <link rel="stylesheet" href="fonts/fonts.css" media="all">
      ' template/index.html

      sed -i -e '/function \+loadGoogleFontCss.*{/ {
        :l; N; /^\( *\).*\n\1[^ ]/!bl; d
      }' -e '/loadGoogleFontCss/d' -e '/webfontloader/d' template/main.js

      echo "checking whether anything refers to Google URLs..." >&2
      ! grep -ri '//[^ ]*google' template
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
