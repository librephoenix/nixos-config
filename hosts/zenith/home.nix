{ config, lib, pkgs, ... }:

{
  config = {

    userSettings = {
      # setup
      shell = {
        enable = true;
        apps.enable = true;
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

    wayland.windowManager.hyprland = lib.mkIf config.userSettings.hyprland.enable {
      settings = {
        bind = [
          ''SUPER,E,exec,if hyprctl clients | grep qutegmail; then echo "scratch_mail respawn not needed"; else qutebrowser --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=4 -B ~/.browser/Teaching :'set input.mode_override passthrough -u mail.google.com' :'set window.title_format qutegmail' :'set tabs.show never' :'set statusbar.show never' https://mail.google.com; fi''
          "SUPER,E,togglespecialworkspace,scratch_email"
        ];

        windowrulev2 = [
          "float,title:^(qutegmail)$"
          "size 80% 85%,title:^(qutegmail)$"
          "workspace special:scratch_email ,title:^(qutegmail)$"
          "center,title:^(qutegmail)$"
        ];

        monitor = [
          "eDP-1,1920x1080,1920x0,1"
          "HDMI-A-1,1920x1080,0x0,1"
        ];

      };
    };
  };
}
