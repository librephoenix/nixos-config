{ config, pkgs, ... }:

{
  config = {
    nix = {
      package = pkgs.nix;
      settings = {
        substituters = [
          "https://cache.nixos.org"
          "https://hyprland.cachix.org"
          "https://nix-community.cachix.org"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
        trusted-users = config.systemSettings.adminUsers ++ [ "@wheel" ];
        auto-optimise-store = true;
        download-buffer-size = 500000000;
      };
    };
    programs.nix-ld = {
      enable = true;
      #Include libstdc++ in the nix-ld profile
      libraries = [
        pkgs.stdenv.cc.cc
        pkgs.zlib
        pkgs.fuse3
        pkgs.icu
        pkgs.nss
        pkgs.openssl
        pkgs.curl
        pkgs.expat
        pkgs.xorg.libX11
        pkgs.vulkan-headers
        pkgs.vulkan-loader
        pkgs.vulkan-tools
      ];
    };
    system.stateVersion = "22.11";
  };
}
