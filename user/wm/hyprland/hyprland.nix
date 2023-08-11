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
