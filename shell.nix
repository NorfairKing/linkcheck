let
  pkgs = import ./nix/pkgs.nix;
  pre-commit = import ./nix/pre-commit.nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "sydtest-shell";
  buildInputs = with pkgs; [
    niv
    zlib
  ] ++ pre-commit.tools;
  shellHook = ''
    export TMPDIR=/tmp
    ${pre-commit.check.shellHook}
  '';
}
