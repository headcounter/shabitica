{ lib, mongodb, systemd }:

mongodb.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ [ ./mongodb-systemd.patch ];
  buildInputs = (drv.buildInputs or []) ++ [ systemd ];
  NIX_LDFLAGS = lib.toList (drv.NIX_LDFLAGS or []) ++ [ "-lsystemd" ];
})
