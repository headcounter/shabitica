{ lib, mongodb, systemd }:

mongodb.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ [ ./systemd.patch ];
  buildInputs = (drv.buildInputs or []) ++ [ systemd ];
  NIX_LDFLAGS = lib.toList (drv.NIX_LDFLAGS or []) ++ [ "-lsystemd" ];
})
