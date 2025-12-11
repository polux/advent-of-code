let
  nixpkgs_version = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz";
    sha256 = "sha256:1915r28xc4znrh2vf4rrjnxldw2imysz819gzhk9qlrkqanmfsxd";
  };
  pkgs = import nixpkgs_version { };
in

with pkgs;

let
  adventOfCodeData = python311.pkgs.buildPythonPackage rec {
    pname = "advent-of-code-data";
    version = "2.1.0";
    src = fetchFromGitHub {
      owner = "wimglenn";
      repo = "advent-of-code-data";
      rev = "v${version}";
      sha256 = "sha256-xR9CfyOUsKSSA/1zYi6kCK3oAaX6Kd625mKMWI+ZFMA=";
    };
    propagatedBuildInputs = [
      python311.pkgs.requests
      python311.pkgs.beautifulsoup4
      python311.pkgs.pebble
      python311.pkgs.aocd-example-parser
      python311.pkgs.browser-cookie3
    ];
    pyproject = true;
    build-system = [ python311.pkgs.setuptools ];
  };

  aocd_python_env = python311.withPackages (ps: [
    adventOfCodeData
  ]);
in

stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [
    gnumake
    gcc
    pkg-config
    z3
    ripgrep
    difftastic
    haskell.compiler.ghc984
    haskell-language-server
    cabal-install
    ghcid
    cbc
  ];
  buildInputs = [
    pcre
    gmp
    libglvnd
    libGL
    libGLU
    freeglut
    zlib
    lazygit
    aocd_python_env
  ];
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
}
