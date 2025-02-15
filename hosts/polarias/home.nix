{ config, lib, pkgs, ... }:

{
  config = {

    userSettings = {
      # setup
      shell = {
        enable = true;
        apps.enable = true;
        extraApps.enable = true;
      };
      xdg.enable = true;

      # programs
      browser = "qutebrowser";
      brave.enable = true;
      editor = "emacs";
      ranger.enable = true;
      git.enable = true;
      engineering.enable = true;
      art.enable = true;
      flatpak.enable = false;
      godot.enable = true;
      keepass.enable = true;
      media.enable = true;
      music.enable = true;
      office.enable = true;

      # wm
      hyprland.enable = true;

      # style
      stylix.enable = true;

      # hardware
      bluetooth.enable = true;
    };

    wayland.windowManager.hyprland = lib.mkIf config.userSettings.hyprland.enable {
      settings = {
        monitor = [
          "eDP-1,1920x1080@300,900x1080,1"
          "HDMI-A-1,1920x1080,1920x0,1"
          "DP-1,1920x1080,0x0,1"
        ];

      };
    };


  };
}
