{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, hspec, http-types
      , mime-mail, neat-interpolation, network, stache, stdenv, systemd
      , text, unordered-containers, vector, wai, warp, word-wrap
      }:
      mkDerivation {
        pname = "shabitica-mailer";
        version = "0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base containers http-types mime-mail network stache systemd
          text unordered-containers vector wai warp word-wrap
        ];
        testHaskellDepends = [ hspec neat-interpolation ];
        description = "Habitica email helper";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
