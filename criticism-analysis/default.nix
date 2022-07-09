with import 
  (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2022-04-16";
    url = "https://github.com/nixos/nixpkgs/";
    # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "75ad56bdc927f3a9f9e05e3c3614c4c1fcd99fcb";
}) {};

( let
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
      click
      nltk
      matplotlib
      pandas
      jupyter
      scikitlearn
      # newAltair
      dominate
      textMatcher
      termcolor
      jupyterlab # Dev
      # flake8  # Dev
      # python-language-server
      # pyls-mypy
      toolz
    ];
  }).env
