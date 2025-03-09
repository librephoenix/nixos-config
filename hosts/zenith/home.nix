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

    home.sessionVariables = lib.mkIf config.userSettings.hyprland.enable {
      AQ_DRM_DEVICES = lib.mkForce "/dev/dri/card0";
    };

    wayland.windowManager.hyprland = lib.mkIf config.userSettings.hyprland.enable {
      settings = {
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
          "eDP-1,1920x1080,1536x0,1.25"
          "HDMI-A-1,1920x1080,0x0,1.25"
        ];

      };
    };

    services.fnott.settings = {
      main = {
        output = "eDP-1"; # notifications only on main display
      };
    };

    services.nextcloud-client = {
      enable = lib.mkForce false;
      startInBackground = lib.mkForce false;
    };

    home.file.".config/hypr/hypridle.conf".text = lib.mkForce ''
      general {
        lock_cmd = pgrep hyprlock || hyprlock
        before_sleep_cmd = loginctl lock-session
        ignore_dbus_inhibit = false
      }

      listener {
        timeout = 3000 # in seconds
        on-timeout = loginctl lock-session
      }
      listener {
        timeout = 3015 # in seconds
        on-timeout = systemctl suspend
        on-resume = hyprctl dispatch dpms on
      }
    '';

  };
}
