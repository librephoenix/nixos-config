{ config, lib, pkgs, ... }:

{
  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [];
    settings = {};
    extraConfig = ''
      bind=SUPER,RETURN,exec,alacritty
      bind=SUPER,A,exec,emacs
      bind=SUPER,S,exec,librewolf
      bind=SUPER,code:47,exec,wofi --show drun
      xwayland {
        force_zero_scaling = true
      }
      monitor=eDP-1,1920x1080,1000x1200,1
      monitor=HDMI-A-1,1920x1200,1920x0,1
      monitor=DP-1,1920x1200,0x0,1
      input {
        kb_layout = us
        kb_options = caps:escape
        repeat_delay = 350
        repeat_rate = 50
      }
    '';
    xwayland = {
      enable = true;
    };
    systemdIntegration = true;
  };

  home.packages = with pkgs; [
    alacritty
    kitty
    feh
    killall
    polkit_gnome
    libva-utils
    gsettings-desktop-schemas
    swaynotificationcenter
    wlr-randr
    ydotool
    hyprland-share-picker
    wl-clipboard
    hyprland-protocols
    hyprpicker
    swayidle
    swaylock
    hyprpaper
    wofi
    swww
    grim
    qt5.qtwayland
    qt6.qtwayland
    xdg-utils
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
  ];
}
