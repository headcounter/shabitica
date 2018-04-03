{ nixpkgs ? <nixpkgs> }:

let
  pkgs = import nixpkgs {};
  inherit (pkgs) lib;

  jobs = {
    tests = import ./tests.nix { inherit nixpkgs pkgs; };

    manual = let
      modules = import "${nixpkgs}/nixos/lib/eval-config.nix" {
        modules = [ ./. ];
        check = false;
      };

      isHOpt = opt: lib.head (lib.splitString "." opt.name) == "habitica";
      filterDoc = lib.filter (opt: isHOpt opt && opt.visible && !opt.internal);
      filtered = filterDoc (lib.optionAttrSetToDocList modules.options);
      optsXML = builtins.unsafeDiscardStringContext (builtins.toXML filtered);
      optsFile = builtins.toFile "options.xml" optsXML;

    in pkgs.stdenv.mkDerivation {
      name = "habitica-options-manual";

      nativeBuildInputs = [ pkgs.libxslt ];

      buildCommand = ''
        dest="$out/share/doc/habitica"
        mkdir -p "$dest"

        cat > manual.xml <<XML
        <book xmlns="http://docbook.org/ns/docbook"
              xmlns:xlink="http://www.w3.org/1999/xlink"
              xmlns:xi="http://www.w3.org/2001/XInclude">
          <title>NixOS options for Habitica Standalone</title>
          <xi:include href="options-db.xml" />
        </book>
        XML

        xsltproc -o options-db.xml \
          "${nixpkgs}/nixos/doc/manual/options-to-docbook.xsl" \
          ${lib.escapeShellArg optsFile}

        xsltproc -o "$dest/index.html" -nonet -xinclude \
          --param section.autolabel 1 \
          --param section.label.includes.component.label 1 \
          --stringparam html.stylesheet style.css \
          --param xref.with.number.and.title 1 \
          --stringparam admon.style "" \
          ${pkgs.docbook5_xsl}/xml/xsl/docbook/xhtml/docbook.xsl \
          manual.xml

        cp "${nixpkgs}/nixos/doc/manual/style.css" "$dest/style.css"

        mkdir -p "$out/nix-support"
        echo "doc manual $dest" > "$out/nix-support/hydra-build-products"
      '';
    };
  };

in jobs // {
  habitica = pkgs.releaseTools.channel {
    name = "habitica";
    constituents = lib.collect lib.isDerivation jobs;
    src = ./.;
  };
}
