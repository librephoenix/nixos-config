{ inputs, pkgs, pkgs-stable, config, lib, ... }:

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
        (sddm-chili-theme.override {
          themeConfig = {
            background = config.stylix.image;
            ScreenWidth = 1920;
            ScreenHeight = 1080;
            blur = true;
            recursiveBlurLoops = 3;
            recursiveBlurRadius = 5;
            # TODO fix icons with svgs patched from stylix colors
          };})
    ];

    # Display manager
    services.xserver.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      enableHidpi = true;
      theme = "chili";
      package = pkgs.libsForQt5.sddm;
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
      jack.enable = true;
    };

    # Some fancy fonts
    fonts.packages = with pkgs-stable; [
      # Fonts
      nerdfonts
      powerline
    ];

    # Auto rotate screen
    programs.iio-hyprland = {
      enable = true;
      package = pkgs.iio-hyprland.overrideAttrs {
        patches = [ ./iio-hyprland-hyprpaper.patch ];
      };
    };
  };
}
