{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib
, nodePackages ? import ../generated { inherit pkgs; }
}:

let
  # The Git revision of https://github.com/google/fonts to use.
  rev = "37a6a68abbe589b387edba33fc6edda129a62047";

  # Info about font weights and sizes can be looked up in the METADATA.pb files
  # from the upstream repository at https://github.com/google/fonts.
  fonts = {
    index = [
      { family = "Roboto";
        style = "normal";
        weight = 400;
        fullName = "Roboto";
        source = "apache/roboto/Roboto-Regular.ttf";
        sha256 = "15bdh52jl469fbdqwib5ayd4m0j7dljss8ixdc8c5njp8r053s3r";
      }
      { family = "Roboto";
        style = "italic";
        weight = 400;
        fullName = "Roboto Italic";
        source = "apache/roboto/Roboto-Italic.ttf";
        sha256 = "1jlzgzpr5l38v48zfzbyn0mygf52aqfj8pjk1ngx3x59idpqpkjz";
      }
      { family = "Roboto";
        style = "normal";
        weight = 700;
        fullName = "Roboto Bold";
        source = "apache/roboto/Roboto-Bold.ttf";
        sha256 = "11pzmyd0fzvknqk056k7bsi00v7apv98rsnp3bq7mgp0wcg9j2vx";
      }
      { family = "Roboto";
        style = "italic";
        weight = 700;
        fullName = "Roboto Bold Italic";
        source = "apache/roboto/Roboto-BoldItalic.ttf";
        sha256 = "0m0kw176c8rg22jni7yvlg24zvf3a6ya963ij7h7va8ryhaarg54";
      }
      { family = "Roboto Condensed";
        style = "normal";
        weight = 400;
        fullName = "Roboto Condensed";
        source = "apache/robotocondensed/RobotoCondensed-Regular.ttf";
        sha256 = "1f7l5q9w2040kyfi9wx6wfb4y2cld0jlncgx4rsxf2fxc5zgnb3a";
      }
      { family = "Roboto Condensed";
        style = "italic";
        weight = 400;
        fullName = "Roboto Condensed Italic";
        source = "apache/robotocondensed/RobotoCondensed-Italic.ttf";
        sha256 = "0a1zfnlhlwj5mrxk5dyi32xlcp74hi7w39k9c02nj5yin4pc271m";
      }
      { family = "Roboto Condensed";
        style = "normal";
        weight = 700;
        fullName = "Roboto Condensed Bold";
        source = "apache/robotocondensed/RobotoCondensed-Bold.ttf";
        sha256 = "1vs6j684qladrx11yzi8hdfjsgsqxlsag3s294rvzx4s5c2fid03";
      }
      { family = "Roboto Condensed";
        style = "italic";
        weight = 700;
        fullName = "Roboto Condensed Bold Italic";
        source = "apache/robotocondensed/RobotoCondensed-BoldItalic.ttf";
        sha256 = "03hrgvkfm6r5qqvqyl11vhs97zl4lfigf6ngdcshcgp4dwjrkjxf";
      }
    ];
    home = lib.singleton {
      family = "Varela Round";
      style = "normal";
      weight = 400;
      fullName = "Varela Round Regular";
      source = "ofl/varelaround/VarelaRound-Regular.ttf";
      sha256 = "1ca6l7fvw8rirgg9r3d3hgm01sghd5ax61iwr4r82m7wp4l6ldls";
    };
    apidoc = [
      { family = "Source Code Pro";
        style = "normal";
        weight = 400;
        fullName = "Source Code Pro Regular";
        source = "ofl/sourcecodepro/SourceCodePro-Regular.ttf";
        sha256 = "0lrp921b1b8cvblj74gj97yin4bw87b41myhz4nnimxz2d0dhc4r";
      }
      { family = "Source Sans Pro";
        style = "normal";
        weight = 400;
        fullName = "Source Sans Pro Regular";
        source = "ofl/sourcesanspro/SourceSansPro-Regular.ttf";
        sha256 = "0alrp7gp8w2syvly8mn9jvdrxmv7mi9pwilb1jf5ljn5nj30mlbi";
      }
    ];
  };

  mkFonts = name: fonts: let
    mkFont = { family, style, weight, fullName, source, sha256 }: rec {
      familyName = lib.replaceStrings [" "] ["_"] family;
      baseName = "${familyName}-${style}-${toString weight}";

      src = pkgs.fetchurl {
        name = "${baseName}.ttf";
        url = "https://github.com/google/fonts/blob/${rev}/${source}?raw=true";
        inherit sha256;
      };

      css = ''
        @font-face {
          font-family: '${family}';
          font-style: ${style};
          font-weight: ${toString weight};
          src: url(${baseName}.eot);
          src: local('${fullName}'),
               url(${baseName}.eot?#iefix) format('embedded-opentype'),
               url(${baseName}.woff2) format('woff2'),
               url(${baseName}.woff) format('woff'),
               url(${baseName}.ttf) format('ttf'),
               url(${baseName}.svg#${baseName}) format('svg');
        }
      '';
    };

    mkFontCommand = font: let
      result = mkFont font;
      baseNameEsc = lib.escapeShellArg result.baseName;
    in ''
      cp ${lib.escapeShellArg result.src} ${baseNameEsc}.ttf

      ttf2svg ${baseNameEsc}.ttf ${baseNameEsc}.svg
      ttf2eot ${baseNameEsc}.ttf ${baseNameEsc}.eot
      ttf2woff ${baseNameEsc}.ttf ${baseNameEsc}.woff
      woff2_compress ${baseNameEsc}.ttf

      for ext in ttf eot svg woff woff2; do
        install -m 0644 -vD ${baseNameEsc}."$ext" "$out"/${baseNameEsc}."$ext"
      done
      echo -n ${lib.escapeShellArg result.css} >> "$out/fonts.css"
    '';

  in pkgs.runCommand "${name}-fonts" {
    nativeBuildInputs = [
      nodePackages.extra.ttf2svg
      nodePackages.extra.ttf2eot
      nodePackages.extra.ttf2woff
      pkgs.woff2
    ];
  } (lib.concatMapStrings mkFontCommand fonts);

in lib.mapAttrs mkFonts fonts
