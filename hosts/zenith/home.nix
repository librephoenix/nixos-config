{
  config,
  lib,
  pkgs,
  pkgs-stable,
  ...
}:

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
      remote.enable = true;
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

    home.packages = with pkgs; [
      openldap
      ldapvi
      rclone
      teams-for-linux
      #rpi-imager # FIXME
      freerdp
      pkgs-stable.tigervnc
      unixtools.xxd
      nodejs
      electron
    ];

    home.sessionVariables = lib.mkIf config.userSettings.hyprland.enable {
      AQ_DRM_DEVICES = lib.mkForce "/dev/dri/card1:/dev/dri/card0";
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

        monitor = [
          "eDP-1,1920x1200,1536x0,1.25"
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
