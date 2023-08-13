{ config, lib, pkgs, ... }:

{
  imports = [
    ../../app/terminal/alacritty.nix
    ../../app/terminal/kitty.nix
    ( import ../../app/dmenu-scripts/networkmanager-dmenu.nix {dmenu_command = "wofi --show dmenu"; inherit config lib pkgs;})
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [];
    settings = {};
    extraConfig = ''
      general {
        layout = master
      }
      bind=SUPER,SPACE,fullscreen,1
      bind=ALT,TAB,cyclenext
      bind=ALTSHIFT,TAB,cyclenext,prev
      bind=SUPER,RETURN,exec,alacritty
      bind=SUPER,A,exec,emacs
      bind=SUPER,S,exec,librewolf
      bind=SUPER,code:47,exec,fuzzel
      bind=SUPER,Q,killactive
      bind=SUPERSHIFT,Q,exit
      bind=SUPER,mouse:272,movewindow
      bind=SUPER,mouse:273,movewindow

      bind=SUPER,H,movefocus,l
      bind=SUPER,J,movefocus,d
      bind=SUPER,K,movefocus,u
      bind=SUPER,L,movefocus,r

      bind=SUPERSHIFT,H,movewindow,l
      bind=SUPERSHIFT,J,movewindow,d
      bind=SUPERSHIFT,K,movewindow,u
      bind=SUPERSHIFT,L,movewindow,r

      bind=SUPER,1,workspace,1
      bind=SUPER,2,workspace,2
      bind=SUPER,3,workspace,3
      bind=SUPER,4,workspace,4
      bind=SUPER,5,workspace,5
      bind=SUPER,6,workspace,6
      bind=SUPER,7,workspace,7
      bind=SUPER,8,workspace,8
      bind=SUPER,9,workspace,9
      bind=SUPER,0,workspace,10

      bind=SUPERSHIFT,1,movetoworkspace,1
      bind=SUPERSHIFT,2,movetoworkspace,2
      bind=SUPERSHIFT,3,movetoworkspace,3
      bind=SUPERSHIFT,4,movetoworkspace,4
      bind=SUPERSHIFT,5,movetoworkspace,5
      bind=SUPERSHIFT,6,movetoworkspace,6
      bind=SUPERSHIFT,7,movetoworkspace,7
      bind=SUPERSHIFT,8,movetoworkspace,8
      bind=SUPERSHIFT,9,movetoworkspace,9
      bind=SUPERSHIFT,0,movetoworkspace,10

      bind=SUPERCTRL,right,workspace,+1
      bind=SUPERCTRL,left,workspace,-1

      bind=SUPER,I,exec,networkmanager_dmenu
      bind=SUPER,P,exec,keepmenu

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
        accel_profile = adaptive
        follow_mouse = 2
      }
    env = WLR_DRM_DEVICES,/dev/dri/card1:/dev/dri/card0
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
    #wofi
    fuzzel
    wev
    swww
    grim
    qt5.qtwayland
    qt6.qtwayland
    xdg-utils
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    wlsunset
    (pkgs.writeScriptBin "sct" ''
      #!/bin/sh
      killall wlsunset
      temphigh=$(( $1 + 1 ))
      templow=$1
      wlsunset -t $templow -T $temphigh &
    '')
    (pkgs.python3Packages.buildPythonPackage rec {
      pname = "pyprland";
      version = "1.4.0";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "sha256-gB/QkTbkr9VMzPPhubQNorPTS4Lm90TS1LCSGqPzPmU=";
      };
      format = "pyproject";
      propagatedBuildInputs = with pkgs; [
        python3Packages.setuptools
        python3Packages.poetry-core
        poetry
      ];
      doCheck = false;
    })
  ];
}
