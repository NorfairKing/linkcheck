{ sources ? import ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
in
pkgsv {
  overlays =
    [
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
