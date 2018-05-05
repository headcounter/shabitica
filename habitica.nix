{ lib, pkgs, stdenv, makeWrapper, writeText, jq, nodejs-8_x

, habiticaConfig ? {
    NODE_ENV = "production";
    BASE_URL = "http://localhost";
    ADMIN_EMAIL = "unconfigured@example.org";
  }
}:

let
  nodePackages = callPackage ./node-packages.nix {};

  callPackage = lib.callPackageWith (pkgs // {
    inherit callPackage nodePackages habiticaConfig;
  });

  src = callPackage ./source.nix {};
  inherit (src) version;

  mkCommonBuild = attrs: let
    filteredAttrs = removeAttrs attrs [
      "name" "version" "nativeBuildInputs" "buildInputs"
    ];
  in stdenv.mkDerivation ({
    name = "habitica-${attrs.name}-${version}";
    inherit src version;

    configurePhase = ''
      runHook preConfigure

      # Merge config.json.example with our config and create config.json:
      echo ${lib.escapeShellArg (builtins.toJSON habiticaConfig)} \
        | ${jq}/bin/jq -s '.[0] * .[1]' config.json.example - > config.json

      runHook postConfigure
    '';

    buildProg = "gulp";

    buildPhase = let
      mkVar = key: val: "${key}=${lib.escapeShellArg (toString val)}";
      env = lib.concatStringsSep " " (lib.mapAttrsToList mkVar habiticaConfig);
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

    nativeBuildInputs = [ nodejs-8_x ] ++ (attrs.nativeBuildInputs or []);

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

  migrator = mkCommonBuild {
    name = "migrator";

    inherit (server) runtimeNodePath;

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
        's!\(\.\./\)\+website/server!${server}/libexec/habitica/server!' {} +
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
    '' + lib.concatStrings (lib.imap1 (num: { file, ... }: ''
      migPath=${lib.escapeShellArg "_migrations_transpiled/${file}"}
      migFile="$(basename "$migPath")"
      migDir="_migrations/${toString num}"
      migOutDir="$out/share/habitica/migrations/${toString num}"
      migRunner="$migDir/runner.js"

      mkdir "$migDir"

      substitute _migrations_transpiled/migration-runner.js "$migRunner" \
        --subst-var-by migrationScript "$migOutDir/$migFile"

      # Move the actual migration script so that we get a build failure if we
      # have duplicate migrations.
      mv "$migPath" "$migDir/$migFile"

      makeWrapper \
        ${lib.escapeShellArg "${nodejs-8_x}/bin/node"} "$migDir/run.sh" \
        --add-flags "$migOutDir/runner.js" \
        --set NODE_ENV production \
        --set NODE_PATH "$runtimeNodePath" \
        --run "cd '${server}/libexec/habitica'" \
        --run ${let
          desc = "${migrationMsg} ${toString num} (${file})";
        in lib.escapeShellArg "echo ${lib.escapeShellArg desc} >&2"}
    '') (import ./migrations.nix));

    installPhase = ''
      mkdir -p "$out/bin" "$out/share/habitica"

      mv _migrations "$out/share/habitica/migrations"

      cat > "$out/bin/migrate" <<EOF
      #!${stdenv.shell} -e
      usage() {
        echo "Usage \$0 MIGRATION_NUMBER" >&2
        echo "Run the specified database migration number on Habitica" >&2
      }

      if [ -z "\$1" ]; then
        usage; exit 1
      else
        exec "$out/share/habitica/migrations/\$1/run.sh" "\$@"
      fi
      EOF
      chmod +x "$out/bin/migrate"
    '';
  };

  server = mkCommonBuild {
    name = "server";

    buildTarget = "build:server";

    postPatch = ''
      # Load config.json from $out/etc/habitica:
      sed -i -e '/^const PATH_TO_CONFIG/ {
        c const PATH_TO_CONFIG = "'"$out"'/etc/habitica/config.json";
      }' website/server/libs/setupNconf.js

      # Hardcode the data of the client's index.html.
      indexData="$(sed -e 's/[\\'\''']/\\&/g' "${client}/index.html")"
      substituteInPlace website/server/libs/client.js \
        --subst-var-by CLIENT_INDEX_DATA "$indexData"
    '';

    nativeBuildInputs = [ makeWrapper ];

    runtimeNodePath = let
      packages = lib.attrValues nodePackages.main;
    in lib.makeSearchPath "lib/node_modules" packages;

    installPhase = ''
      mkdir -p "$out/bin" "$out/libexec/habitica"
      install -vD -m 0644 config.json "$out/etc/habitica/config.json"
      cp -rdT website/transpiled-babel "$out/libexec/habitica/server"
      cp -rdT website/common/transpiled-babel "$out/libexec/habitica/common"
      cp -rdT website/common/locales "$out/libexec/habitica/common/locales"

      makeWrapper \
        ${lib.escapeShellArg "${nodejs-8_x}/bin/node"} \
        "$out/bin/habitica-server" \
        --add-flags "$out/libexec/habitica/server/index.js" \
        --set NODE_ENV production \
        --set NODE_PATH "$runtimeNodePath" \
        --run "cd '$out/libexec/habitica'"
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
