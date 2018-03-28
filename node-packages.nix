{ stdenv, lib, callPackage, pkgs }:

let
  # We need to remove package-lock.json files for all node modules, otherwise
  # npm will try to fetch those dependencies from the registry.
  removeLock = package: package.overrideAttrs (drv: {
    preRebuild = (drv.preRebuild or "") + ''
      find -name package-lock.json -delete
    '';
  });

  mkOverrides = cat: packages: let
    withoutLocks = lib.mapAttrs (lib.const removeLock) packages;
    overrides = (callPackage ./node-overrides.nix {}).${cat} or {};
    mkOverride = olist: let
      listLen = lib.length olist;
      override = (lib.head olist).overrideAttrs (lib.last olist);
    in if listLen == 1 then lib.head olist
       else if listLen == 2 then override
       else throw "Unknown override list with length ${toString listLen}.";
  in lib.zipAttrsWith (lib.const mkOverride) [ withoutLocks overrides ];

in lib.mapAttrs mkOverrides (import ./generated {
  inherit (stdenv) system;
  inherit pkgs;
})
