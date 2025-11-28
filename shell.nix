with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [ gnumake gcc pkg-config z3 ripgrep difftastic haskell.compiler.ghc984 haskell-language-server cabal-install ];
  buildInputs = [ pcre gmp libglvnd libGL libGLU freeglut zlib lazygit ];
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
}
