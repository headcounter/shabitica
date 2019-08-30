{ runCommand, mongodb-tools, mongodb, makeWrapper

, socketPath ? "/run/shabitica/db.sock"
}:

let
  patchedTools = mongodb-tools.overrideAttrs (drv: {
    postPatch = (drv.postPatch or "") + ''
      sed -i -e '
        s!\(net\.DialTimeout(\)"tcp",[^,]*!\1"unix", "${socketPath}"!
      ' common/db/connector.go
    '';
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
    --add-flags "mongodb://$socketPath/admin"
''
