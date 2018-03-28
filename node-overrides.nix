{ lib, fetchFromGitHub
, libsass, libjpeg, optipng, gifsicle, pkgconfig, phantomjs2

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
    newerSass = libsass.overrideAttrs (drv: rec {
      name = "libsass-${version}";
      version = assert lib.versionOlder drv.version "3.5.0"; "3.5.0";
      patchPhase = "export LIBSASS_VERSION=${version}";
      src = fetchFromGitHub {
        owner = "sass";
        repo = "libsass";
        rev = version;
        sha256 = "06ch6af7ivmx1f5l3z3dx3iqdiiwis1vcc1j4wdm10f6mlq11yxx";
      };
    });
  in {
    LIBSASS_EXT = "auto";
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [ pkgconfig ];
    buildInputs = (drv.buildInputs or []) ++ [ newerSass ];
  };

  main.pageres = drv: {
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [ phantomjs2 ];
    # XXX: For PhantomJS 2 with NixOS 17.09:
    QT_QPA_PLATFORM = "offscreen";
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
}
