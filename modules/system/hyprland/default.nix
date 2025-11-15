{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.systemSettings.hyprland;
in
{
  options = {
    systemSettings.hyprland = {
      enable = lib.mkEnableOption "Enable hyprland";
    };
  };

  config = lib.mkIf cfg.enable {
    # Power key should not shut off computer by defaultPower key shuts of
    services.logind.powerKey = "suspend";

    # Hyprland
    programs = {
      hyprland = {
        enable = true;
        package = inputs.hyprland.packages.${pkgs.system}.hyprland;
        xwayland = {
          enable = true;
        };
        portalPackage = pkgs.xdg-desktop-portal-hyprland;
      };
    };

    # Necessary packages
    environment.systemPackages = with pkgs; [
      jq
      (sddm-astronaut.override {
        themeConfig = {
          # TODO Update Theme Config
          # https://github.com/Keyitdev/sddm-astronaut-theme/blob/master/Themes/astronaut.conf
          background = config.stylix.image;
          ScreenWidth = 1920;
          ScreenHeight = 1080;
          blur = false;
        };
      })
    ];

    # Display manager
    services.xserver.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      enableHidpi = true;
      theme = "sddm-astronaut-theme";
      package = pkgs.kdePackages.sddm;
      extraPackages = with pkgs; [
        (sddm-astronaut.override {
          themeConfig = {
            background = config.stylix.image;
            ScreenWidth = 1920;
            ScreenHeight = 1080;
            blur = false;
          };
        })
      ];
    };

    services.upower.enable = true;

    # Define systemd service to run on boot to load avatars for sddm
    systemd.services."sddm-avatar" = {
      description = "Service to copy or update users Avatars at startup.";
      wantedBy = [ "multi-user.target" ];
      before = [ "sddm.service" ];
      script = ''
        set -eu
        mkdir -p /var/lib/AccountsService/icons
        for user in /home/*; do
            username=$(basename "$user")
            if [ -f "$user/.face.icon" ]; then
                if [ ! -f "/var/lib/AccountsService/icons/$username" ]; then
                    cp "$user/.face.icon" "/var/lib/AccountsService/icons/$username"
                else
                    if [ "$user/.face.icon" -nt "/var/lib/AccountsService/icons/$username" ]; then
                        cp "$user/.face.icon" "/var/lib/AccountsService/icons/$username"
                    fi
                fi
            fi
        done
      '';
      serviceConfig = {
        Type = "simple";
        User = "root";
        StandardOutput = "journal+console";
        StandardError = "journal+console";
      };
    };

    # # Ensures SDDM starts after the service.
    systemd.services.sddm = {
      after = [ "sddm-avatar.service" ];
    };

    # xwayland
    services.xserver = {
      enable = true;
      xkb = {
        layout = "us";
        variant = "";
        options = "caps:escape";
      };
      excludePackages = [ pkgs.xterm ];
    };

    # Keyring
    security.pam.services.login.enableGnomeKeyring = true;
    services.gnome.gnome-keyring.enable = true;

    # Dbus
    services.dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    programs.dconf.enable = true;

    # Pipewire
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    # Auto rotate screen
    programs.iio-hyprland = {
      enable = true;
      package = pkgs.iio-hyprland.overrideAttrs {
        patches = [ ./iio-hyprland-hyprpaper.patch ];
      };
    };
  };
}
