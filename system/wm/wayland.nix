{ config, lib, pkgs, ... }:

{
  imports = [ ./pipewire.nix
              ./dbus.nix
              ./gnome-keyring.nix
              ./fonts.nix
            ];

  environment.systemPackages = [ pkgs.wayland ];

  # Configure xwayland
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
    xkbOptions = "caps:escape";
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
  };
}
