{ lib, pkgs, stdenv, makeWrapper, writeText, jq

, shabiticaConfig ? {
    NODE_ENV = "production";
    BASE_URL = "http://localhost";
    ADMIN_EMAIL = "unconfigured@example.org";
  }
}:

let
  # XXX: Backwards-compatibility for NixOS 18.03.
  nodejs = pkgs.nodejs-10_x or (let
    buildSrc = "${toString pkgs.path}/pkgs/development/web/nodejs/nodejs.nix";
  in (pkgs.callPackage buildSrc {
    openssl = pkgs.openssl_1_1_0;
    libuv = pkgs.libuv.overrideAttrs (lib.const rec {
      version = "1.23.1";
      name = "libuv-${version}";

      src = pkgs.fetchFromGitHub {
        owner = "libuv";
        repo = "libuv";
        rev = "v${version}";
        sha256 = "14h8dcyx81sbckbgmqhagncyz8s6z6qzpx0fy8p79whq5hb3f4jg";
      };
    });
  }) {
    enableNpm = true;
    version = "10.12.0";
    patches = [];
    sha256 = "1r0aqcxafha13ks8586x77n77zi88db259cpaix0y1ivdh6qkkfr";
  });

  nodePackages = callPackage ./node-packages.nix {
    inherit nodejs;
  };

  callPackage = lib.callPackageWith (pkgs // {
    inherit callPackage nodePackages shabiticaConfig;
  });

  src = callPackage ./source.nix {};
  inherit (src) version;

  mkCommonBuild = attrs: let
    filteredAttrs = removeAttrs attrs [
      "name" "version" "nativeBuildInputs" "buildInputs"
    ];
  in stdenv.mkDerivation ({
    name = "shabitica-${attrs.name}-${version}";
    inherit src version;

    configurePhase = ''
      runHook preConfigure

      # Merge config.json.example with our config and create config.json:
      echo ${lib.escapeShellArg (builtins.toJSON shabiticaConfig)} \
        | ${jq}/bin/jq -s '.[0] * .[1]' config.json.example - > config.json

      runHook postConfigure
    '';

    buildProg = "gulp";

    buildPhase = let
      mkVar = key: val: "${key}=${lib.escapeShellArg (toString val)}";
      vars = lib.mapAttrsToList mkVar shabiticaConfig;
      env = lib.concatStringsSep " " vars;
    in ''
      runHook preBuild

      if ! HOME="$PWD" ${env} $buildProg $buildTarget; then
        if [ -n "$createHydraTestFailure" ]; then
          mkdir -p "$out/nix-support"
          touch "$out/nix-support/failed"
        else
          exit 1
        fi
      fi

      runHook postBuild
    '';

    nativeBuildInputs = [ nodejs ] ++ (attrs.nativeBuildInputs or []);

    buildInputs = (lib.attrValues nodePackages.main) ++ [
      nodePackages.dev.babel-plugin-istanbul
      nodePackages.dev.babel-plugin-syntax-object-rest-spread
    ] ++ (attrs.buildInputs or []);
  } // removeAttrs attrs [
    "name" "version" "nativeBuildInputs" "buildInputs"
  ]);

  inherit (import ./docinfo.nix) migrationMsg;

in rec {
  client = mkCommonBuild {
    name = "client";

    buildTarget = "build:client";

    # XXX: Ugly as fuck and only needed for webpack!
    postConfigure = ''
      mkdir node_modules
      find ''${NODE_PATH//:/ } -mindepth 1 -maxdepth 1 -type d \
        \( -name .bin -o -exec ln -sft node_modules {} + \)
    '';

    # XXX: Dependency lookup hell ahead, this is clearly something we want to
    #      get rid of, especially because we use buildInputs here (which will
    #      fail once we add anything but node packages to it).
    NODE_PATH = let
      needsSubdep = p: lib.elem p.packageName [ "svg-url-loader" "webpack" ];
      mkSubdep = p: "${p}/lib/node_modules/${p.packageName}/node_modules";
      eligible = lib.filter needsSubdep (lib.attrValues nodePackages.main);
    in lib.concatMapStringsSep ":" mkSubdep eligible;

    installPhase = ''
      cp -rdT dist-client "$out"
    '';
  };

  migrator = let
    mkMigStep = ident: { file, ... }: ''
      migPath=${lib.escapeShellArg "_migrations_transpiled/${file}"}
      migFile="$(basename "$migPath")"
      migDir="_migrations/${toString ident}"
      migOutDir="$out/share/shabitica/migrations/${toString ident}"
      migRunner="$migDir/runner.js"

      mkdir "$migDir"

      substitute _migrations_transpiled/migration-runner.js "$migRunner" \
        --subst-var-by migrationScript "$migOutDir/$migFile"

      # Move the actual migration script so that we get a build failure if we
      # have duplicate migrations.
      mv "$migPath" "$migDir/$migFile"

      makeWrapper \
        ${lib.escapeShellArg "${nodejs}/bin/node"} "$migDir/run.sh" \
        --add-flags "$migOutDir/runner.js" \
        --set NODE_ENV production \
        --set NODE_PATH "$runtimeNodePath" \
        --run "cd '${server}/libexec/shabitica'" \
        --run ${let
          desc = "${migrationMsg} ${toString ident} (${file})";
        in lib.escapeShellArg "echo ${lib.escapeShellArg desc} >&2"}
    '';
  in mkCommonBuild {
    name = "migrator";

    runtimeNodePath = let
      monk = "${nodePackages.dev.monk}/lib/node_modules";
    in "${server.runtimeNodePath}:${monk}";

    nativeBuildInputs = [ makeWrapper ];

    preBuild = ''
      sed -i -e '
        /babel-register/d
        /require.\+website\/server\/server/c \
          require("../website/server/libs/setupMongoose"); \
          require("../website/server/models/challenge"); \
          require("../website/server/models/group"); \
          require("../website/server/models/user");
      ' migrations/migration-runner.js

      find migrations -type f -name '*.js' -exec sed -i -e \
        's!\(\.\./\)\+website/server!${server}/libexec/shabitica/server!' {} +
    '';

    buildProg = "gulp --cwd . -f ${writeText "build-migrations.js" ''
      const gulp = require('gulp');
      const babel = require('gulp-babel');

      gulp.task('build:migrations', () => {
        return gulp.src('migrations/**/*.js')
          .pipe(babel())
          .pipe(gulp.dest('_migrations_transpiled/'));
      });
    ''}";

    buildTarget = "build:migrations";

    postBuild = ''
      # Note that we don't use -p or anything like that, because we want this
      # to fail if the directory already exists.
      mkdir _migrations

      # All of the regular migrations
      ${lib.concatStrings (lib.imap1 mkMigStep (import ./migrations.nix))}
      # Special one to restock Armoire
      ${mkMigStep "armoire" { file = "restock_armoire.js"; }}

      # Gets a deterministic SHA256 of all the Armoire items available in order
      # to compare it with the state of the running system and reset the
      # armoireEmpty user flag if needed.
      HOME="$PWD" node <<EOF | sha256sum | cut -d' ' -f1 > armoire.sha256
      require('babel-register');
      const armoire =
        require('./website/common/script/content/gear/sets/armoire.js');
      console.log(Object.keys(armoire)
        .sort()
        .map(k => k + '=' + Object.keys(armoire[k]).sort().join(','))
        .join(';'));
      EOF
    '';

    installPhase = ''
      mkdir -p "$out/bin" "$out/share/shabitica"
      mv _migrations "$out/share/shabitica/migrations"
      install -m 0644 -vD armoire.sha256 "$out/share/shabitica/armoire.sha256"

      cat > "$out/bin/maybe-restock-armoire" <<EOF
      #!${stdenv.shell} -e
      if [ -e /var/lib/shabitica/armoire.sha256 ]; then
        if [ "\$(< /var/lib/shabitica/armoire.sha256)" \
           = "$(< armoire.sha256)" ]; then
          exit 0
        fi
      fi
      exec "$out/share/shabitica/migrations/armoire/run.sh"
      EOF
      chmod +x "$out/bin/maybe-restock-armoire"

      cat > "$out/bin/migrate" <<EOF
      #!${stdenv.shell} -e
      usage() {
        echo "Usage \$0 MIGRATION_ID" >&2
        echo "Run the specified database migration ID on Shabitica" >&2
      }

      if [ -z "\$1" ]; then
        usage; exit 1
      else
        exec "$out/share/shabitica/migrations/\$1/run.sh" "\$@"
      fi
      EOF
      chmod +x "$out/bin/migrate"
    '';
  };

  server = mkCommonBuild {
    name = "server";

    buildTarget = "build:server";

    postPatch = ''
      # Load config.json from $out/etc/shabitica:
      sed -i -e '/^const PATH_TO_CONFIG/ {
        c const PATH_TO_CONFIG = "'"$out"'/etc/shabitica/config.json";
      }' website/server/libs/setupNconf.js

      # Hardcode the data of the client's index.html.
      indexData="$(sed -e 's/[\\'\''']/\\&/g' "${client}/index.html")"
      substituteInPlace website/server/libs/client.js \
        --subst-var-by CLIENT_INDEX_DATA "$indexData"

      # Fix include path for common/errors:
      sed -i -e '/import/s,/\.\.,,' \
        website/common/script/libs/errorMessage.js
    '';

    nativeBuildInputs = [ makeWrapper ];

    runtimeNodePath = let
      packages = lib.attrValues nodePackages.main;
    in lib.makeSearchPath "lib/node_modules" packages;

    installPhase = ''
      mkdir -p "$out/bin" "$out/libexec/shabitica"
      install -vD -m 0644 config.json "$out/etc/shabitica/config.json"
      cp -rdT website/transpiled-babel "$out/libexec/shabitica/server"
      cp -rdT website/common/transpiled-babel "$out/libexec/shabitica/common"
      cp -rdT website/common/locales "$out/libexec/shabitica/common/locales"
      cp -rdT website/common/errors "$out/libexec/shabitica/common/errors"

      makeWrapper \
        ${lib.escapeShellArg "${nodejs}/bin/node"} \
        "$out/bin/shabitica-server" \
        --add-flags "$out/libexec/shabitica/server/index.js" \
        --set NODE_ENV production \
        --set NODE_PATH "$runtimeNodePath" \
        --run "cd '$out/libexec/shabitica'"
    '';
  };

  apidoc = mkCommonBuild {
    name = "apidoc";
    buildTarget = "apidoc";
    installPhase = "cp -rdT apidoc_build \"$out\"";
  };

  inherit mkCommonBuild nodePackages;
  source = src;
}
