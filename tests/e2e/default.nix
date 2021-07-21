{ system ? builtins.currentSystem
, nixpkgs ? toString <nixpkgs> # XXX: pkgs.path
, pkgs ? import nixpkgs { inherit system; config = {}; }
, lib ? pkgs.lib

, reruns ? 0
}:

let
  python = import deps/requirements.nix { inherit pkgs; };

  # XXX: NixOS 18.09 doesn't have ip2unix, so provide a minimal build.
  ip2unix = pkgs.ip2unix or (pkgs.stdenv.mkDerivation rec {
    name = "ip2unix-${version}";
    version = "2.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "nixcloud";
      repo = "ip2unix";
      rev = "v${version}";
      sha256 = "1hbc7csicgza1w43aq1q189w8gzwkc2v5mxpnhaiwggwsa5vvkl0";
    };
    nativeBuildInputs = [ pkgs.meson pkgs.ninja pkgs.pkg-config ];
    buildInputs = [ pkgs.libyamlcpp ];
  });

  buildVM = let
    buildVMsFun = import "${nixpkgs}/nixos/lib/build-vms.nix";
    # XXX: Backwards-compatibility for NixOS 18.09, there is no pkgs argument.
    args = if builtins.functionArgs buildVMsFun ? pkgs then {
      inherit system pkgs;
    } else {
      inherit system;
    };
  in (buildVMsFun args).buildVM;

  server = buildVM {} (lib.singleton {
    imports = [ ../../. ];
    networking.firewall.enable = false;
    networking.hostName = "server";
    shabitica.hostName = "shabitica.example.org";
    shabitica.useSSL = false;
    shabitica.config.INVITE_ONLY = 0;
    nix.readOnlyStore = true;
    virtualisation.diskSize = 16384;
    virtualisation.memorySize = 2048;
    virtualisation.writableStore = false;
    virtualisation.qemu.options = [ "-device virtio-rng-pci" ];
    virtualisation.qemu.networkingOptions = let
      portFws = [ "hostfwd=tcp:127.0.0.1:8100-:80" ];
    in [
      "-device virtio-net-pci,netdev=vlan0"
      "-netdev user,id=vlan0,${lib.concatStringsSep "," portFws}"
    ];
  });

in pkgs.runCommand "shabitica-tests-e2e" {
  tests = lib.cleanSource ./tests;

  nativeBuildInputs = let
    firefoxBinary = "${pkgs.firefox-unwrapped}/lib/firefox/firefox";
    firefox = pkgs.writeScriptBin "firefox" ''
      #!${pkgs.stdenv.shell}
      export HOME="$(mktemp -d)"
      exec ${lib.escapeShellArg firefoxBinary} "$@"
    '';
  in [
    server.config.system.build.vm

    (pkgs.writeScriptBin "run-in-guest" ''
      #!${pkgs.stdenv.shell}
      exec ${lib.escapeShellArg pkgs.python3.interpreter} \
           ${./run-in-guest.py} "$@"
    '')

    python.packages.pytest-rerunfailures
    python.packages.pytest-selenium
    python.packages.pytest-xdist
    ip2unix

    pkgs.geckodriver
    firefox
  ];

  FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = []; };

  libredirect = "${pkgs.libredirect.overrideAttrs (drv: {
    postPatch = "sed -i -e /unsetenv/d libredirect.c";
  })}/lib/libredirect.so";
  NIX_REDIRECTS = "/etc/hosts=${pkgs.writeText "test-hosts" ''
    127.0.0.1 localhost shabitica.example.org
    ::1 localhost shabitica.example.org
  ''}";

} ''
  export RUNNER_BASEDIR="$PWD"
  run-in-guest

  ip2unix -r addr=127.0.0.1,port=8100,path=shabitica.sock \
    run-server-vm -nographic \
    -chardev "socket,id=shell,path=$RUNNER_BASEDIR/shell" \
    -device virtio-serial -device virtconsole,chardev=shell &

  # Wait for multi-user.target to become active.
  while ! run-in-guest systemctl --no-pager show multi-user.target \
    | grep -qx ActiveState=active; do sleep 1; done

  if [ "$NIX_BUILD_CORES" -gt 8 ]; then
    concurrency=8
  else
    concurrency="$NIX_BUILD_CORES"
  fi
  export SELENIUM_CAPTURE_DEBUG=always
  if ! \
    LD_PRELOAD="$libredirect" \
    ip2unix -r out,addr=127.0.0.1,port=80,path=shabitica.sock \
            -r out,port=53,reject \
    pytest -v -n "$concurrency" --reruns ${toString reruns} \
      --html="$out/report.html" --driver Firefox \
      -o cache_dir="$PWD" "$tests"
  then
    echo "ERROR: Test suite has failed, output is in $out/report.html" >&2
    mkdir -p "$out/nix-support"
    # XXX: Hydra doesn't yet support a content-addressable store for output
    #      failures.
    touch "$out/nix-support/failed"
  fi

  mkdir -p "$out/nix-support"
  echo "report test-report $out report.html" \
    > "$out/nix-support/hydra-build-products"
''
