{ runCommand, mongodb-tools, makeWrapper

, socketPath ? "/run/habitica/db.sock"
}:

let
  patchedTools = mongodb-tools.overrideAttrs (drv: {
    postPatch = (drv.postPatch or "") + ''
      sed -i -e '
        s!\(net\.DialTimeout(\)"tcp",[^,]*!\1"unix", "${socketPath}"!
      ' common/db/connector.go
    '';
  });

in runCommand "habitica-db-tools" {
  nativeBuildInputs = [ makeWrapper ];
} ''
  mkdir -p "$out/bin"
  for i in dump restore export import; do
    makeWrapper "${patchedTools}/bin/mongo$i" "$out/bin/habitica-db-$i"
  done
''
