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

  src = callPackage ./source.nix {};
  inherit (src) version;

  common = {
    configurePhase = ''
      # Merge config.json.example with our config and create config.json:
      echo ${lib.escapeShellArg (builtins.toJSON habiticaConfig)} \
        | ${jq}/bin/jq -s '.[0] * .[1]' config.json.example - > config.json

      runHook postConfigure
    '';

    buildPhase = let
      mkVar = key: val: "${key}=${lib.escapeShellArg (toString val)}";
      env = lib.concatStringsSep " " (lib.mapAttrsToList mkVar habiticaConfig);
    in "HOME=\"$PWD\" ${env} gulp $gulpTarget";

    nativeBuildInputs = [ nodejs-8_x ];
    buildInputs = (lib.attrValues nodePackages.main) ++ [
      nodePackages.dev.babel-plugin-istanbul
      nodePackages.dev.babel-plugin-syntax-object-rest-spread
    ];
  };

in {
  client = stdenv.mkDerivation (common // {
    name = "habitica-client-${version}";
    inherit src version;

    gulpTarget = "build:client";

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
      eligibleInputs = lib.filter needsSubdep common.buildInputs;
    in lib.concatMapStringsSep ":" mkSubdep eligibleInputs;

    installPhase = ''
      cp -rdT dist-client "$out"
    '';
  });

  server = stdenv.mkDerivation (common // {
    name = "habitica-server-${version}";
    inherit src version;

    gulpTarget = "build:server";

    postPatch = ''
      # Load config.json from $out/etc/habitica:
      sed -i -e '/^const PATH_TO_CONFIG/ {
        c const PATH_TO_CONFIG = "'"$out"'/etc/habitica/config.json";
      }' website/server/libs/setupNconf.js
    '';

    nativeBuildInputs = common.nativeBuildInputs ++ [ makeWrapper ];

    runtimeNodePath = let
      packages = lib.attrValues nodePackages.main;
    in lib.makeSearchPath "lib/node_modules" packages;

    installPhase = ''
      mkdir -p "$out/bin" "$out/libexec/habitica"
      install -vD -m 0644 config.json "$out/etc/habitica/config.json"
      cp -rdT website/transpiled-babel "$out/libexec/habitica/transpiled-babel"
      cp -rdT website/common "$out/libexec/habitica/common"

      makeWrapper \
        ${lib.escapeShellArg "${nodejs-8_x}/bin/node"} \
        "$out/bin/habitica-server" \
        --add-flags "$out/libexec/habitica/transpiled-babel/index.js" \
        --set NODE_ENV production \
        --set NODE_PATH "$runtimeNodePath" \
        --run "cd '$out/libexec/habitica'"
    '';
  });

  apidoc = stdenv.mkDerivation (common // {
    name = "habitica-apidoc-${version}";
    inherit src version;

    gulpTarget = "apidoc";

    installPhase = ''
      cp -rdT apidoc_build "$out"
    '';
  });
}
