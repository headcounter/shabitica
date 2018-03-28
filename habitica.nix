{ lib, pkgs, stdenv, makeWrapper, jq, nodejs-8_x

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

in stdenv.mkDerivation rec {
  name = "habitica-${version}";
  inherit (src) version;

  src = callPackage ./source.nix {};

  configurePhase = ''
    # Merge config.json.example with our config and create config.json:
    echo ${lib.escapeShellArg (builtins.toJSON habiticaConfig)} \
      | jq -s '.[0] * .[1]' config.json.example - > config.json

    # Load config.json from $out/etc/habitica:
    sed -i -e '/^const PATH_TO_CONFIG/ {
      c const PATH_TO_CONFIG = "'"$out"'/etc/habitica/config.json";
    }' website/server/libs/setupNconf.js

    # XXX: Ugly as fuck and only needed for webpack!
    mkdir node_modules
    find ''${NODE_PATH//:/ } -mindepth 1 -maxdepth 1 -type d \
      \( -name .bin -o -exec ln -sft node_modules {} + \)
  '';

  # XXX: Dependency lookup hell ahead, this is clearly something we want to get
  #      rid of, especially because we use buildInputs here (which will fail
  #      once we add anything but node packages to it).
  NODE_PATH = let
    needsSubdep = p: lib.elem p.packageName [ "svg-url-loader" "webpack" ];
    mkSubdep = p: "${p}/lib/node_modules/${p.packageName}/node_modules";
  in lib.concatMapStringsSep ":" mkSubdep (lib.filter needsSubdep buildInputs);

  buildInputs = (lib.attrValues nodePackages.main) ++ [
    nodePackages.dev.babel-plugin-istanbul
    nodePackages.dev.babel-plugin-syntax-object-rest-spread
  ];

  nativeBuildInputs = [ nodejs-8_x jq makeWrapper ];

  buildPhase = let
    mkVar = key: val: "${key}=${lib.escapeShellArg (toString val)}";
    env = lib.concatStringsSep " " (lib.mapAttrsToList mkVar habiticaConfig);
  in "HOME=\"$PWD\" ${env} gulp build";

  runtimeNodePath = let
    packages = lib.attrValues nodePackages.main;
  in lib.makeSearchPath "lib/node_modules" packages;

  installPhase = ''
    mkdir -p "$out/bin" "$out/libexec/habitica" "$out/share/habitica"
    install -vD -m 0644 config.json "$out/etc/habitica/config.json"
    cp -rdT website/transpiled-babel "$out/libexec/habitica/transpiled-babel"
    cp -rdT website/common "$out/libexec/habitica/common"
    cp -rdT dist-client "$out/share/habitica/client"
    cp -rdT apidoc_build "$out/share/habitica/apidoc"

    makeWrapper \
      ${lib.escapeShellArg "${nodejs-8_x}/bin/node"} \
      "$out/bin/habitica-server" \
      --add-flags "$out/libexec/habitica/transpiled-babel/index.js" \
      --set NODE_ENV production \
      --set NODE_PATH "$runtimeNodePath" \
      --run "cd '$out/libexec/habitica'"
  '';
}
