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
      recording.enable = true;
      virtualization = {
        virtualMachines.enable = true;
      };

      # wm
      hyprland.enable = true;

      # style
      stylix.enable = true;

      # hardware
      bluetooth.enable = true;
    };

    home.sessionVariables = lib.mkIf config.userSettings.hyprland.enable {
      AQ_DRM_DEVICES = lib.mkForce "/dev/dri/card0:/dev/dri/card1";
    };

    wayland.windowManager.hyprland = lib.mkIf config.userSettings.hyprland.enable {
      settings = {
        monitor = [
          "eDP-1,1920x1080@300,720x864,1.25,vrr,0"
          "HDMI-A-1,1920x1080,1536x0,1.25,vrr,0"
          "DP-1,1920x1080,0x0,1.25,vrr,0"
        ];
        misc = {
          vrr = 1;
        };
        experimental = {
          xx_color_management_v4 = true;
        };

      };
    };
  };
}
