{ lib, runCommand, mongodb-tools, mongodb, makeWrapper

, socketPath ? "/run/shabitica/db.sock"
}:

let
  # TODO: Drop after we no longer support NixOS 19.03.
  toolsIsV4 = lib.versionAtLeast (lib.getVersion mongodb-tools) "4";
  socketPathEsc = lib.replaceStrings [ "/" ] [ "%2f" ] socketPath;

  patchedTools = mongodb-tools.overrideAttrs (drv: {
    postPatch = (drv.postPatch or "") + (if toolsIsV4 then ''
      sed -i -e '
        s!"localhost"!"${socketPathEsc}"!
      ' vendor/github.com/mongodb/mongo-tools-common/util/mongo.go
    '' else ''
      sed -i -e '
        s!\(net\.DialTimeout(\)"tcp",[^,]*!\1"unix", "${socketPath}"!
      ' common/db/connector.go
    '');
  });

in runCommand "shabitica-db-tools" {
  nativeBuildInputs = [ makeWrapper ];
  inherit socketPath;
} ''
  mkdir -p "$out/bin"
  for i in dump restore export import; do
    makeWrapper "${patchedTools}/bin/mongo$i" "$out/bin/shabitica-db-$i"
  done

  makeWrapper "${mongodb}/bin/mongo" "$out/bin/shabitica-db-shell" \
    --add-flags "--host '$socketPath' admin"
''
