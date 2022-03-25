with import <nixpkgs> {};

( let
    newAltair = pkgs.python3Packages.buildPythonPackage rec {
      pname = "altair";
      version = "4.1.0";

      src = pkgs.python3Packages.fetchPypi{
        inherit version; inherit pname;
        sha256 = "0c99q5dy6f275yg1f137ird08wmwc1z8wmvjickkf2mvyka31p9y";
      };

      buildInputs = with pkgs.python3Packages; [
        entrypoints
        jinja2
        jsonschema
        numpy
        toolz
        pandas
      ];
      doCheck = false;

    };
    textMatcher = pkgs.python3Packages.buildPythonPackage rec {
      pname = "text_matcher";
      version = "0.1.6";
      src = pkgs.fetchFromGitHub {
        owner = "JonathanReeve";
        repo = "text-matcher";
        rev = "53fb290b50d4e984257540abf78589f77504ff65";
        sha256 = "0l2uQ8XHlHfgkewLhH0ucDvCEDq4s9daz/+JxMeplb4=";
      };
      buildInputs = with pkgs.python3Packages; [
        click
        nltk
        termcolor
      ];
      doCheck = false;
    };
in pkgs.python3.buildEnv.override rec {
    extraLibs = with pkgs.python3Packages; [
      matplotlib
      pandas
      jupyter
      scikitlearn
      newAltair
      dominate
      textMatcher
      jupyterlab # Dev
      # flake8  # Dev
      # python-language-server
      # pyls-mypy
      toolz
    ];
  }).env
