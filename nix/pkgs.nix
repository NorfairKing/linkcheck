let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  linkCheckPkgs =
    pkgsv {
      overlays =
        [
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
linkCheckPkgs
