{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, sourceTree ? null }:

let
  workDir = let
    noArgErr = "You need to specify a 'sourceTree' argument with something"
             + " like '--arg sourceTree ~/habitica'.";
  in if sourceTree == null then throw noArgErr else toString sourceTree;

  shabitica = pkgs.callPackages pkgs/shabitica {
    shabiticaConfig = rec {
      NODE_ENV = "development";
      SESSION_SECRET = "YOUR SECRET HERE";
      SESSION_SECRET_KEY = "12345678912345678912345678912345"
                         + "67891234567891234567891234567891";
      SESSION_SECRET_IV = "12345678912345678912345678912345";
      NODE_DB_URI = "mongodb://%2Ftmp%2Fdb.sock";
      BASE_URL = "http://localhost:3000";
      ADMIN_EMAIL = "unconfigured@example.org";
      INVITE_ONLY = false;
    };
  };

  mongoDbCfg = pkgs.writeText "mongodb.conf" (builtins.toJSON {
    net.bindIp = "/tmp/db.sock";
    processManagement.fork = false;
    storage.dbPath = "/tmp/db";
    storage.engine = "ephemeralForTest";
    storage.journal.enabled = false;
  });

  mkShellDrv = attrs: shabitica.mkCommonBuild ({
    name = "dev-shell";
    nativeBuildInputs = lib.attrValues shabitica.nodePackages.dev
                     ++ lib.singleton pkgs.mongodb;
  } // attrs);

  # A very hacky way to gather the build-time dependencies of "mkShellDrv {}".
  # See https://github.com/NixOS/nix/issues/1245
  shellDependencies = let
    drv = builtins.readFile (mkShellDrv {}).drvPath;
    # The only regex-unfriendly characters a store dir could have is a dot.
    storeDirRe = lib.replaceStrings [ "." ] [ "\\." ] builtins.storeDir;
    storeBaseRe = "[0-9a-df-np-sv-z]{32}-[+_?=a-zA-Z0-9-][+_?=.a-zA-Z0-9-]*";
    re = "(${storeDirRe}/${storeBaseRe}\\.drv)";
    inputs = lib.concatLists (lib.filter lib.isList (builtins.split re drv));
  in map import inputs;

  shellClosure = pkgs.runCommand "shell-closure" {
    exportReferencesGraph =
      lib.imap0 (n: d: [ "closure-${toString n}" d ]) shellDependencies;
  } ''
    runtimeDeps="$(sed -ne '
      p; n; n

      :cdown
      /^0*$/b
      :l; s/0\(X*\)$/X\1/; tl

      s/^\(X*\)$/9\1/; tdone
      ${lib.concatMapStrings (num: ''
        s/${toString num}\(X*\)$/${toString (num - 1)}\1/; tdone
      '') (lib.range 1 9)}

      :done
      y/X/9/
      x; n; p; x
      bcdown
    ' closure-* | sort -u)"
    echo "$runtimeDeps" > "$out"
  '';

  vuizvuiSrc = pkgs.fetchFromGitHub {
    owner = "openlab-aux";
    repo = "vuizvui";
    rev = "38e417120d9045534e77bfa20a27c293b718ffd8";
    sha256 = "1481v640jas4kb6qp93j8k6jz0v7sl77phna57830lnqphpcxry6";
  };

  environment = mkShellDrv {
    buildPhase = ":";
    installPhase = ''
      install -vD -m 0644 config.json "$out/config.json"
      declare -p -x NODE_PATH PATH > "$out/env.sh"
    '';
  };

  shellHook = let
    wrapped = ''
      killEverything() {
        retry=0
        while kill -0 $(jobs -p); do
          if [ $retry -ge 15 ]; then
            kill -9 $(jobs -p)
          else
            kill $(jobs -p)
          fi
          retry=$(($retry + 1))
          ${pkgs.coreutils}/bin/sleep 0.1
        done 2> /dev/null || :
      }

      trap kill_everything EXIT
      set -e

      cd ${lib.escapeShellArg workDir}
      source ${lib.escapeShellArg environment}/env.sh
      mkdir -p /tmp/db
      mongod --config ${mongoDbCfg} &
      npm run client:dev &
      npm start
    '';
  in pkgs.writeScriptBin "shabitica-dev-shell-hook" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.coreutils}/bin/env -i "$SHELL" -e \
      -c ${lib.escapeShellArg wrapped}
  '';

  sandboxedShellHook = pkgs.stdenv.mkDerivation {
    name = "shabitica-dev-sandbox";
    src = "${vuizvuiSrc}/pkgs/build-support/build-sandbox/src";

    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [ pkgs.nix ];
    makeFlags = [ "BINDIR=${shellHook}/bin" ];

    exportReferencesGraph = let
      main = [ "closure-0" shellHook ];
      misc = lib.imap1 (n: d: [ "closure-${toString n}" d ]) shellDependencies;
    in main ++ lib.concatLists misc;

    patchPhase = ''
      sed -i -e '/static char.*get_mount_target/s/^static //' setup.c
    '';

    configurePhase = let
      mkCString = val: "\"${lib.escape ["\\" "\""] val}\"";

      configSource = mkCString "${environment}/config.json";
      configDest = mkCString "${workDir}/config.json";

      paramsFile = pkgs.writeText "params.c" ''
        #include "setup.h"

        #include <dirent.h>
        #include <errno.h>
        #include <fcntl.h>
        #include <stdio.h>
        #include <stdlib.h>
        #include <string.h>
        #include <unistd.h>
        #include <sys/mount.h>

        bool setup_app_paths(void) {
          int fd;
          char *target;
          DIR *workdir;
          struct dirent *wdent;

          @shellDepCode@

          if ((workdir = opendir(${mkCString workDir})) == NULL) {
            fprintf(stderr, "open directory %s: %s\n",
                    ${mkCString workDir}, strerror(errno));
            return false;
          }

          while ((wdent = readdir(workdir)) != NULL) {
            size_t namelen, pathlen;
            char *path;

            if (strcmp(wdent->d_name, "config.json") == 0 ||
                strcmp(wdent->d_name, ".") == 0 ||
                strcmp(wdent->d_name, "..") == 0)
              continue;

            namelen = strlen(wdent->d_name);
            pathlen = ${toString (lib.stringLength workDir)} + namelen + 2;

            if ((path = malloc(pathlen)) == NULL) {
              perror("malloc working path");
              closedir(workdir);
              return false;
            }

            if (snprintf(path, pathlen, "%s/%s", ${mkCString workDir},
                         wdent->d_name) < 0) {
              perror("snprintf working dir entry");
              free(path);
              closedir(workdir);
            }

            if (!bind_mount(path, true, true, true)) {
              free(path);
              closedir(workdir);
              return false;
            }

            free(path);
          }

          closedir(workdir);

          if ((target = get_mount_target(${configDest})) == NULL)
            return false;

          if ((fd = creat(target, 0666)) == -1) {
            fprintf(stderr, "unable to create %s: %s\n",
                    target, strerror(errno));
            free(target);
            return false;
          }

          if (close(fd) == -1) {
            fprintf(stderr, "unable to close %s: %s\n",
                    target, strerror(errno));
            free(target);
            return false;
          }

          if (mount(${configSource}, target, "", MS_BIND, NULL) == -1) {
            fprintf(stderr, "mount file %s to %s: %s\n",
                    ${configSource}, target, strerror(errno));
            free(target);
            return false;
          }

          free(target);
          return true;
        }

        bool mount_runtime_path_vars(struct query_state *qs) {
          return true;
        }
      '';

    in ''
      shellDeps="$(sed -ne '
        p; n; n

        :cdown
        /^0*$/b
        :l; s/0\(X*\)$/X\1/; tl

        s/^\(X*\)$/9\1/; tdone
        ${lib.concatMapStrings (num: ''
          s/${toString num}\(X*\)$/${toString (num - 1)}\1/; tdone
        '') (lib.range 1 9)}

        :done
        y/X/9/
        x; n; p; x
        bcdown
      ' ../closure-[0-9]* | sort -u)"

      shellDepCode="$(
        for dep in $shellDeps; do
          echo 'if (!bind_mount("'"$dep"'", true, true, true)) return false;'
        done
      )"

      substitute ${paramsFile} params.c --subst-var shellDepCode
    '';
  };

in mkShellDrv {
  shellHook = ''
    exec ${sandboxedShellHook}/bin/shabitica-dev-shell-hook
  '';
}
