{ lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "go-camo-${version}";
  version = "1.1.7";

  goPackagePath = "github.com/cactus/go-camo";

  src = fetchFromGitHub {
    owner = "cactus";
    repo = "go-camo";
    rev = "v${version}";
    sha256 = "19fayml2138wk0lkbslwn1yg4bwcw88h09r1iwbr7xf7sjabad8k";
  };

  patches = [
    ./imageproxy-socket-activation.patch
    ./imageproxy-decode-session.patch
    ./imageproxy-remove-metrics.patch
  ];

  extraSrcs = let
    mkGoDep = { goPackagePath, rev, sha256, ... }@attrs: let
      matchRepoOwner = builtins.match "github\\.com/([^/]+)/([^/]+)";
      matchResult = matchRepoOwner goPackagePath;
    in if matchResult == null then attrs else {
      inherit goPackagePath;
      src = fetchFromGitHub {
        repo = lib.last matchResult;
        owner = lib.head matchResult;
        inherit rev sha256;
      };
    };
  in map mkGoDep [
    { goPackagePath = "github.com/cactus/mlog";
      rev = "v1.0.1";
      sha256 = "05iz2a1ynbb9672clvykbvxs63wibyxlsl52wq4k7b53db0b09vh";
    }
    { goPackagePath = "github.com/cactus/tai64";
      rev = "v1.0.0";
      sha256 = "0x4dk5sw4ypiwd7gr5rjzp4avz0sagzs42r3dw852flh3lpvf4ir";
    }
    { goPackagePath = "github.com/jessevdk/go-flags";
      rev = "v1.3.0";
      sha256 = "1jk2k2l10lwrn1r3nxdvbs0yz656830j4khzirw8p4ahs7c5zz36";
    }
    { goPackagePath = "github.com/coreos/go-systemd";
      rev = "v20";
      sha256 = "0jf705z62hy1230ryw9z3686g31v5yc16izcjzcasbawgv5c000v";
    }
  ];
}
