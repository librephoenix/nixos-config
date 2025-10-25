{ config, lib, pkgs, pkgs-stable, ... }:

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
      browser = "brave";
      editor = "emacs";
      yazi.enable = true;
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
      hyprland.performanceOptimizations = true;

      # style
      stylix.enable = true;

      # hardware
      bluetooth.enable = true;
    };

    home.packages = with pkgs; [
      cage
      pkgs-stable.gcompris
      tuxpaint
    ];

    home.sessionVariables = lib.mkIf config.userSettings.hyprland.enable {
      AQ_DRM_DEVICES = lib.mkForce "/dev/dri/card0";
    };

    wayland.windowManager.hyprland = lib.mkIf config.userSettings.hyprland.enable {
      settings = {
        misc = {
          vfr = true;
        };

        animations = {
          enabled = lib.mkForce "no";
        };

        decoration = {
          shadow.enabled = lib.mkForce false;
          blur = {
            size = lib.mkForce 10;
            passes = lib.mkForce 1;
          };
        };

        #input = {
        #  repeat_delay = lib.mkForce 850;
        #  repeat_rate = lib.mkForce 80;
        #};

        monitor = [
          "eDP-1,1920x1080@48,0x0,1"
        ];

      };
    };

  };
}
