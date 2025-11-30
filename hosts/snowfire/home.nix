{
  config,
  lib,
  pkgs,
  ...
}:

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
      editor = "zed";
      emacs.enable = true;
      vscodium.enable = true;
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
      recording.enable = true;
      virtualization = {
        virtualMachines.enable = true;
      };
      ai.enable = false; # local-ai broken in nixpkgs

      # wm
      hyprland.enable = true;

      # style
      stylix.enable = true;

      # hardware
      bluetooth.enable = true;
    };

    home.packages = with pkgs; [
      freerdp
    ];

    home.sessionVariables = lib.mkIf config.userSettings.hyprland.enable {
      AQ_DRM_DEVICES = lib.mkForce "/dev/dri/card2:/dev/dri/card1";
    };

    wayland.windowManager.hyprland = lib.mkIf config.userSettings.hyprland.enable {
      settings = {
        monitor = [
          "eDP-1,1920x1080@60.00,720x864,1.25,vrr,0"
          "HDMI-A-1,preferred,1536x0,1.25,vrr,0"
          "DP-1,preferred,0x0,1.25,vrr,0"
        ];
      };
    };
  };
}
