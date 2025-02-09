{ config, lib, pkgs, inputs, ... }:

let
  caches = import inputs.secrets.caches;
in {
  config = {
    nix = {
      package = lib.mkForce pkgs.nix;
      settings = {
        substituters =
          (lib.optionals (caches ? urls) caches.urls) ++
          [
            "https://cache.nixos.org"
            "https://hyprland.cachix.org"
            "https://nix-community.cachix.org"
          ];
        trusted-public-keys =
          (lib.optionals (caches ? publicKeys) caches.publicKeys) ++
          [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          ];
        trusted-users = [ "@wheel" ];
        auto-optimise-store = true;
        download-buffer-size = 500000000;
      };
    };
    home.stateVersion = "22.11";
  };
}
