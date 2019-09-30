# generated using pypi2nix tool (version: 2.0.0)
# See more at: https://github.com/nix-community/pypi2nix
#
# COMMAND:
#   pypi2nix -V python37 --no-emit-extra-build-inputs -E libxml2 -E libxslt -r requirements.txt -s setuptools-scm
#

{ pkgs ? import <nixpkgs> {},
  overrides ? ({ pkgs, python }: self: super: {})
}:

let

  inherit (pkgs) makeWrapper;
  inherit (pkgs.stdenv.lib) fix' extends inNixShell;

  pythonPackages =
  import "${toString pkgs.path}/pkgs/top-level/python-packages.nix" {
    inherit pkgs;
    inherit (pkgs) stdenv;
    python = pkgs.python37;
  };

  commonBuildInputs = [];
  commonDoCheck = false;

  withPackages = pkgs':
    let
      pkgs = builtins.removeAttrs pkgs' ["__unfix__"];
      interpreterWithPackages = selectPkgsFn: pythonPackages.buildPythonPackage {
        name = "python37-interpreter";
        buildInputs = [ makeWrapper ] ++ (selectPkgsFn pkgs);
        buildCommand = ''
          mkdir -p $out/bin
          ln -s ${pythonPackages.python.interpreter} \
              $out/bin/${pythonPackages.python.executable}
          for dep in ${builtins.concatStringsSep " "
              (selectPkgsFn pkgs)}; do
            if [ -d "$dep/bin" ]; then
              for prog in "$dep/bin/"*; do
                if [ -x "$prog" ] && [ -f "$prog" ]; then
                  ln -s $prog $out/bin/`basename $prog`
                fi
              done
            fi
          done
          for prog in "$out/bin/"*; do
            wrapProgram "$prog" --prefix PYTHONPATH : "$PYTHONPATH"
          done
          pushd $out/bin
          ln -s ${pythonPackages.python.executable} python
          ln -s ${pythonPackages.python.executable} \
              python3
          popd
        '';
        passthru.interpreter = pythonPackages.python;
      };

      interpreter = interpreterWithPackages builtins.attrValues;
    in {
      __old = pythonPackages;
      inherit interpreter;
      inherit interpreterWithPackages;
      mkDerivation = args: pythonPackages.buildPythonPackage (args // {
        nativeBuildInputs = (args.nativeBuildInputs or []) ++ args.buildInputs;
      });
      packages = pkgs;
      overrideDerivation = drv: f:
        pythonPackages.buildPythonPackage (
          drv.drvAttrs // f drv.drvAttrs // { meta = drv.meta; }
        );
      withPackages = pkgs'':
        withPackages (pkgs // pkgs'');
    };

  python = withPackages {};

  generated = self: {
    "apipkg" = python.mkDerivation {
      name = "apipkg-1.5";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/a8/af/07a13b1560ebcc9bf4dd439aeb63243cbd8d374f4f328691470d6a9b9804/apipkg-1.5.tar.gz";
        sha256 = "37228cda29411948b422fae072f57e31d3396d2ee1c9783775980ee9c9990af6";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/apipkg";
        license = licenses.mit;
        description = "apipkg: namespace control and lazy-import mechanism";
      };
    };

    "atomicwrites" = python.mkDerivation {
      name = "atomicwrites-1.3.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/ec/0f/cd484ac8820fed363b374af30049adc8fd13065720fd4f4c6be8a2309da7/atomicwrites-1.3.0.tar.gz";
        sha256 = "75a9445bac02d8d058d5e1fe689654ba5a6556a1dfd8ce6ec55a0ed79866cfa6";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/untitaker/python-atomicwrites";
        license = licenses.mit;
        description = "Atomic file writes.";
      };
    };

    "attrs" = python.mkDerivation {
      name = "attrs-19.1.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/cc/d9/931a24cc5394f19383fbbe3e1147a0291276afa43a0dc3ed0d6cd9fda813/attrs-19.1.0.tar.gz";
        sha256 = "f0b870f674851ecbfbbbd364d6b5cbdff9dcedbc7f3f5e18a6891057f21fe399";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://www.attrs.org/";
        license = licenses.mit;
        description = "Classes Without Boilerplate";
      };
    };

    "certifi" = python.mkDerivation {
      name = "certifi-2019.9.11";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/62/85/7585750fd65599e88df0fed59c74f5075d4ea2fe611deceb95dd1c2fb25b/certifi-2019.9.11.tar.gz";
        sha256 = "e4f3620cfea4f83eedc95b24abd9cd56f3c4b146dd0177e83a21b4eb49e21e50";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://certifi.io/";
        license = licenses.mpl20;
        description = "Python package for providing Mozilla's CA Bundle.";
      };
    };

    "chardet" = python.mkDerivation {
      name = "chardet-3.0.4";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/fc/bb/a5768c230f9ddb03acc9ef3f0d4a3cf93462473795d18e9535498c8f929d/chardet-3.0.4.tar.gz";
        sha256 = "84ab92ed1c4d4f16916e05906b6b75a6c0fb5db821cc65e70cbd64a3e2a5eaae";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/chardet/chardet";
        license = licenses.lgpl3;
        description = "Universal encoding detector for Python 2 and 3";
      };
    };

    "execnet" = python.mkDerivation {
      name = "execnet-1.7.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/5a/61/1b50e0891d9b934154637fdaac88c68a82fd8dc5648dfb04e65937fc6234/execnet-1.7.1.tar.gz";
        sha256 = "cacb9df31c9680ec5f95553976c4da484d407e85e41c83cb812aa014f0eddc50";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."apipkg"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://execnet.readthedocs.io/en/latest/";
        license = licenses.mit;
        description = "execnet: rapid multi-Python deployment";
      };
    };

    "idna" = python.mkDerivation {
      name = "idna-2.8";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/ad/13/eb56951b6f7950cadb579ca166e448ba77f9d24efc03edd7e55fa57d04b7/idna-2.8.tar.gz";
        sha256 = "c357b3f628cf53ae2c4c05627ecc484553142ca23264e593d327bcde5e9c3407";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/kjd/idna";
        license = licenses.bsdOriginal;
        description = "Internationalized Domain Names in Applications (IDNA)";
      };
    };

    "importlib-metadata" = python.mkDerivation {
      name = "importlib-metadata-0.23";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/5d/44/636bcd15697791943e2dedda0dbe098d8530a38d113b202817133e0b06c0/importlib_metadata-0.23.tar.gz";
        sha256 = "aa18d7378b00b40847790e7c27e11673d7fed219354109d0e7b9e5b25dc3ad26";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools-scm"
      ];
      propagatedBuildInputs = [
        self."zipp"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://importlib-metadata.readthedocs.io/";
        license = "Apache Software License";
        description = "Read metadata from Python packages";
      };
    };

    "more-itertools" = python.mkDerivation {
      name = "more-itertools-7.2.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/c2/31/45f61c8927c9550109f1c4b99ba3ca66d328d889a9c9853a808bff1c9fa0/more-itertools-7.2.0.tar.gz";
        sha256 = "409cd48d4db7052af495b09dec721011634af3753ae1ef92d2b32f73a745f832";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/erikrose/more-itertools";
        license = licenses.mit;
        description = "More routines for operating on iterables, beyond itertools";
      };
    };

    "packaging" = python.mkDerivation {
      name = "packaging-19.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/5a/2f/449ded84226d0e2fda8da9252e5ee7731bdf14cd338f622dfcd9934e0377/packaging-19.2.tar.gz";
        sha256 = "28b924174df7a2fa32c1953825ff29c61e2f5e082343165438812f00d3a7fc47";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pyparsing"
        self."six"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pypa/packaging";
        license = licenses.bsdOriginal;
        description = "Core utilities for Python packages";
      };
    };

    "pluggy" = python.mkDerivation {
      name = "pluggy-0.13.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/d7/9d/ae82a5facf2dd89f557a33ad18eb68e5ac7b7a75cf52bf6a208f29077ecf/pluggy-0.13.0.tar.gz";
        sha256 = "fa5fa1622fa6dd5c030e9cad086fa19ef6a0cf6d7a2d12318e10cb49d6d68f34";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools-scm"
      ];
      propagatedBuildInputs = [
        self."importlib-metadata"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pluggy";
        license = licenses.mit;
        description = "plugin and hook calling mechanisms for python";
      };
    };

    "py" = python.mkDerivation {
      name = "py-1.8.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/f1/5a/87ca5909f400a2de1561f1648883af74345fe96349f34f737cdfc94eba8c/py-1.8.0.tar.gz";
        sha256 = "dc639b046a6e2cff5bbe40194ad65936d6ba360b52b3c3fe1d08a82dd50b5e53";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://py.readthedocs.io/";
        license = licenses.mit;
        description = "library with cross-python path, ini-parsing, io, code, log facilities";
      };
    };

    "pyparsing" = python.mkDerivation {
      name = "pyparsing-2.4.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/7e/24/eaa8d7003aee23eda270099eeec754d7bf4399f75c6a011ef948304f66a2/pyparsing-2.4.2.tar.gz";
        sha256 = "6f98a7b9397e206d78cc01df10131398f1c8b8510a2f4d97d9abd82e1aacdd80";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pyparsing/pyparsing/";
        license = licenses.mit;
        description = "Python parsing module";
      };
    };

    "pytest" = python.mkDerivation {
      name = "pytest-5.2.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/e3/89/bdc4ee34896c03e1b42f80a5e1539443df4715f96d45cd7f4118b7c30229/pytest-5.2.0.tar.gz";
        sha256 = "d8ba7be9466f55ef96ba203fc0f90d0cf212f2f927e69186e1353e30bc7f62e5";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools-scm"
      ];
      propagatedBuildInputs = [
        self."atomicwrites"
        self."attrs"
        self."importlib-metadata"
        self."more-itertools"
        self."packaging"
        self."pluggy"
        self."py"
        self."wcwidth"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://docs.pytest.org/en/latest/";
        license = licenses.mit;
        description = "pytest: simple powerful testing with Python";
      };
    };

    "pytest-base-url" = python.mkDerivation {
      name = "pytest-base-url-1.4.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/cd/c1/a66ccb7801139f79084c04711b8ae17f4318fe52f68929b59ec906cd0e52/pytest-base-url-1.4.1.tar.gz";
        sha256 = "7425e8163345494ac7f544e99c6f3e5a08f4228bee5e26013b98c462a4d31f6e";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytest"
        self."requests"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-base-url";
        license = "Mozilla Public License 2.0 (MPL 2.0)";
        description = "pytest plugin for URL based testing";
      };
    };

    "pytest-forked" = python.mkDerivation {
      name = "pytest-forked-1.0.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/30/be/cb5dc4f0fa5ba121943305f4f235dc1a30fae53daac20094ab89f4618578/pytest-forked-1.0.2.tar.gz";
        sha256 = "d352aaced2ebd54d42a65825722cb433004b4446ab5d2044851d9cc7a00c9e38";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytest"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-forked";
        license = licenses.mit;
        description = "run tests in isolated forked subprocesses";
      };
    };

    "pytest-html" = python.mkDerivation {
      name = "pytest-html-2.0.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/c2/79/2fd6182b1814a0fd9f7d03385df0d49d414a117d3cb9e21d6fcd26ab4a13/pytest-html-2.0.0.tar.gz";
        sha256 = "1428837592c94404e4112fbae76f6e512c35aab4b7d663dc3dd6ea58d2979710";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytest"
        self."pytest-metadata"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-html";
        license = "Mozilla Public License 2.0 (MPL 2.0)";
        description = "pytest plugin for generating HTML reports";
      };
    };

    "pytest-metadata" = python.mkDerivation {
      name = "pytest-metadata-1.8.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/12/38/eed3a1e00c765e4da61e4e833de41c3458cef5d18e819d09f0f160682993/pytest-metadata-1.8.0.tar.gz";
        sha256 = "2071a59285de40d7541fde1eb9f1ddea1c9db165882df82781367471238b66ba";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytest"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-metadata";
        license = "Mozilla Public License 2.0 (MPL 2.0)";
        description = "pytest plugin for test session metadata";
      };
    };

    "pytest-rerunfailures" = python.mkDerivation {
      name = "pytest-rerunfailures-7.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/21/44/4662543ba52f26ff00b33b05e7530482f26e59a871043f8fbdf455f7ef7d/pytest-rerunfailures-7.0.tar.gz";
        sha256 = "f3c9cf31339bf87b048c09dadb633a81156fa4899527fffc55cde105d04ed5fd";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytest"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-rerunfailures";
        license = "Mozilla Public License 2.0 (MPL 2.0)";
        description = "pytest plugin to re-run tests to eliminate flaky failures";
      };
    };

    "pytest-selenium" = python.mkDerivation {
      name = "pytest-selenium-1.17.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/81/1b/68bcf2c7698c2283d584976a21466e68acd53cb7f31a93470d79a3fa4a30/pytest-selenium-1.17.0.tar.gz";
        sha256 = "e8034ebabc3b55fad57bfb97e7b0b2137532dbc65f33706e1ce1ed8e547caa1a";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytest"
        self."pytest-base-url"
        self."pytest-html"
        self."pytest-variables"
        self."requests"
        self."selenium"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-selenium";
        license = "Mozilla Public License 2.0 (MPL 2.0)";
        description = "pytest plugin for Selenium";
      };
    };

    "pytest-variables" = python.mkDerivation {
      name = "pytest-variables-1.8.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/1f/b3/6e3e504184d82622443c2a4215483636d08c2be87a24d05005f126447f2f/pytest-variables-1.8.0.tar.gz";
        sha256 = "a475fe559bf3a1cfce5a603f7b4112415e7a76716c99394c088f788f75cd276a";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytest"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-variables";
        license = "Mozilla Public License 2.0 (MPL 2.0)";
        description = "pytest plugin for providing variables to tests/fixtures";
      };
    };

    "pytest-xdist" = python.mkDerivation {
      name = "pytest-xdist-1.29.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/f8/22/075918bc25107fbe6d1bc7d8a68c1a1570e8268ab5e1baff50f8264f8c83/pytest-xdist-1.29.0.tar.gz";
        sha256 = "3489d91516d7847db5eaecff7a2e623dba68984835dbe6cedb05ae126c4fb17f";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."execnet"
        self."pytest"
        self."pytest-forked"
        self."six"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pytest-xdist";
        license = licenses.mit;
        description = "pytest xdist plugin for distributed testing and loop-on-failing modes";
      };
    };

    "requests" = python.mkDerivation {
      name = "requests-2.22.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/01/62/ddcf76d1d19885e8579acb1b1df26a852b03472c0e46d2b959a714c90608/requests-2.22.0.tar.gz";
        sha256 = "11e007a8a2aa0323f5a921e9e6a2d7e4e67d9877e85773fba9ba6419025cbeb4";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."certifi"
        self."chardet"
        self."idna"
        self."urllib3"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://python-requests.org";
        license = licenses.asl20;
        description = "Python HTTP for Humans.";
      };
    };

    "selenium" = python.mkDerivation {
      name = "selenium-3.141.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/ed/9c/9030520bf6ff0b4c98988448a93c04fcbd5b13cd9520074d8ed53569ccfe/selenium-3.141.0.tar.gz";
        sha256 = "deaf32b60ad91a4611b98d8002757f29e6f2c2d5fcaf202e1c9ad06d6772300d";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."urllib3"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/SeleniumHQ/selenium/";
        license = licenses.asl20;
        description = "Python bindings for Selenium";
      };
    };

    "setuptools-scm" = python.mkDerivation {
      name = "setuptools-scm-3.3.3";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/83/44/53cad68ce686585d12222e6769682c4bdb9686808d2739671f9175e2938b/setuptools_scm-3.3.3.tar.gz";
        sha256 = "bd25e1fb5e4d603dcf490f1fde40fb4c595b357795674c3e5cb7f6217ab39ea5";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pypa/setuptools_scm/";
        license = licenses.mit;
        description = "the blessed package to manage your versions by scm tags";
      };
    };

    "six" = python.mkDerivation {
      name = "six-1.12.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/dd/bf/4138e7bfb757de47d1f4b6994648ec67a51efe58fa907c1e11e350cddfca/six-1.12.0.tar.gz";
        sha256 = "d16a0141ec1a18405cd4ce8b4613101da75da0e9a7aec5bdd4fa804d0e0eba73";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/benjaminp/six";
        license = licenses.mit;
        description = "Python 2 and 3 compatibility utilities";
      };
    };

    "urllib3" = python.mkDerivation {
      name = "urllib3-1.25.6";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/ff/44/29655168da441dff66de03952880c6e2d17b252836ff1aa4421fba556424/urllib3-1.25.6.tar.gz";
        sha256 = "9a107b99a5393caf59c7aa3c1249c16e6879447533d0887f4336dde834c7be86";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://urllib3.readthedocs.io/";
        license = licenses.mit;
        description = "HTTP library with thread-safe connection pooling, file post, and more.";
      };
    };

    "wcwidth" = python.mkDerivation {
      name = "wcwidth-0.1.7";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/55/11/e4a2bb08bb450fdbd42cc709dd40de4ed2c472cf0ccb9e64af22279c5495/wcwidth-0.1.7.tar.gz";
        sha256 = "3df37372226d6e63e1b1e1eda15c594bca98a22d33a23832a90998faa96bc65e";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/jquast/wcwidth";
        license = licenses.mit;
        description = "Measures number of Terminal column cells of wide-character codes";
      };
    };

    "zipp" = python.mkDerivation {
      name = "zipp-0.6.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/57/dd/585d728479d97d25aeeb9aa470d36a4ad8d0ba5610f84e14770128ce6ff7/zipp-0.6.0.tar.gz";
        sha256 = "3718b1cbcd963c7d4c5511a8240812904164b7f381b647143a89d3b98f9bcd8e";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools-scm"
      ];
      propagatedBuildInputs = [
        self."more-itertools"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/jaraco/zipp";
        license = "UNKNOWN";
        description = "Backport of pathlib-compatible object wrapper for zip files";
      };
    };
  };
  localOverridesFile = ./requirements_override.nix;
  localOverrides = import localOverridesFile { inherit pkgs python; };
  commonOverrides = [
    
  ];
  paramOverrides = [
    (overrides { inherit pkgs python; })
  ];
  allOverrides =
    (if (builtins.pathExists localOverridesFile)
     then [localOverrides] else [] ) ++ commonOverrides ++ paramOverrides;

in python.withPackages
   (fix' (pkgs.lib.fold
            extends
            generated
            allOverrides
         )
   )