with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [ gnumake gcc pkg-config z3 ripgrep difftastic ];
  buildInputs = [ pcre gmp libglvnd libGL libGLU freeglut zlib lazygit ];
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
}
