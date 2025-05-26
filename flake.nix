{
  description = "linkcheck";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , safe-coloured-text
    , autodocodec
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;

    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.linkcheck;
      checks.${system} = {
        release = self.packages.${system}.default;
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "linkcheck-shell";
        packages = p: [ p.linkcheck ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
