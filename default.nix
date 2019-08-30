{ pkgs ? null, lib ? null, ... }@args:

# We want our channel name to be as plain and simple as possible, so the
# channel name should be just 'shabitica'. Unfortunately when using nix-env
# with -qa, it tries to auto-call all the Nix expressions in nix-defexpr
# which by default contains channels and thus also the 'shabitica' channel.
#
# Auto-calling our default.nix however doesn't work because we're not
# delivering a set of packages but a NixOS module, so what we're doing here
# is returning an empty attribute set whenever this module is auto-called so
# that "nix-env -qa" will work again.
#
# NOTE: The check on 'lib' is intentional here, because if we'd reference
#       'config' or 'pkgs', we'd short-circuit the fixpoint of the module
#       system and thus run into an infinite recursion.
if lib == null then {} else (import ./modules args)
