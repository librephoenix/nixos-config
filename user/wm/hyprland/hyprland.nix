{ config, lib, pkgs, browser, term, spawnEditor, font, ... }:

{
  imports = [
    ../../app/terminal/alacritty.nix
    ../../app/terminal/kitty.nix
    ( import ../../app/dmenu-scripts/networkmanager-dmenu.nix {dmenu_command = "fuzzel -d"; inherit config lib pkgs;})
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [];
    settings = {};
    extraConfig = ''
      exec-once = pypr
      exec-once = nm-applet
      exec-once = GOMAXPROCS=1 syncthing --no-browser
      exec-once = protonmail-bridge --noninteractive
      exec-once = gnome-keyring-daemon --daemonize --login
      exec-once = gnome-keyring-daemon --start --components=secrets

      exec-once = ~/.swayidle-stylix
      exec = ~/.swaybg-stylix

      general {
        layout = master
      }
      bind=SUPER,SPACE,fullscreen,1
      bind=ALT,TAB,cyclenext
      bind=ALTSHIFT,TAB,cyclenext,prev

      bind=SUPER,RETURN,exec,''+term+''

      bind=SUPER,A,exec,''+spawnEditor+''

      bind=SUPER,S,exec,''+browser+''

      bind=SUPER,code:47,exec,fuzzel
      bind=SUPER,X,exec,fnottctl dismiss
      bind=SUPERSHIFT,X,exec,fnottctl dismiss all
      bind=SUPER,Q,killactive
      bind=SUPERSHIFT,Q,exit
      bind=SUPER,mouse:272,movewindow
      bind=SUPER,mouse:273,movewindow

      bind=,code:107,exec,grim -g "$(slurp)"
      bind=SHIFT,code:107,exec,grim -g "$(slurp -o)"
      bind=SUPER,code:107,exec,grim
      bind=CTRL,code:107,exec,grim -g "$(slurp)" - | wl-copy
      bind=SHIFTCTRL,code:107,exec,grim -g "$(slurp -o)" - | wl-copy
      bind=SUPERCTRL,code:107,exec,grim - | wl-copy

      bind=,code:122,exec,pamixer -d 10
      bind=,code:123,exec,pamixer -i 10
      bind=,code:121,exec,pamixer -t
      bind=,code:256,exec,pamixer --default-source -t
      bind=SHIFT,code:122,exec,pamixer --default-source -d 10
      bind=SHIFT,code:123,exec,pamixer --default-source -i 10
      bind=,code:232,exec,brightnessctl set 15-
      bind=,code:233,exec,brightnessctl set +15
      bind=,code:237,exec,brightnessctl --device='asus::kbd_backlight' set 1-
      bind=,code:238,exec,brightnessctl --device='asus::kbd_backlight' set +1
      bind=,code:255,exec,airplane-mode

      bind=SUPERSHIFT,S,exec,systemctl suspend
      bind=SUPERSHIFT,L,exec,swaylock --indicator-radius 200 --screenshots --effect-blur 10x10

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

      bind=SUPER,Z,exec,pypr toggle term
      bind=SUPER,F,exec,pypr toggle ranger
      bind=SUPER,N,exec,pypr toggle musikcube
      bind=SUPER,B,exec,pypr toggle btm
      bind=SUPER,E,exec,pypr toggle geary
      $scratchpadsize = size 80% 85%

      $scratchpad = class:^(scratchpad)$
      windowrulev2 = float,$scratchpad
      windowrulev2 = $scratchpadsize,$scratchpad
      windowrulev2 = workspace special silent,$scratchpad
      windowrulev2 = center,$scratchpad

      $gearyscratchpad = class:^(geary)$
      windowrulev2 = float,$gearyscratchpad
      windowrulev2 = $scratchpadsize,$gearyscratchpad
      windowrulev2 = workspace special silent,$gearyscratchpad
      windowrulev2 = center,$gearyscratchpad

      bind=SUPER,code:21,exec,pypr zoom
      bind=SUPER,code:21,exec,hyprctl reload

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
    wlr-randr
    wtype
    hyprland-share-picker
    wl-clipboard
    hyprland-protocols
    hyprpicker
    swayidle
    swaylock-effects
    swaybg
    fnott
    #hyprpaper
    #wofi
    fuzzel
    keepmenu
    pinentry_gnome
    wev
    grim
    slurp
    qt5.qtwayland
    qt6.qtwayland
    xdg-utils
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    wlsunset
    pavucontrol
    pamixer
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
  home.file.".config/hypr/pyprland.json".text = ''
    {
      "pyprland": {
        "plugins": ["scratchpads", "magnify"]
      },
      "scratchpads": {
        "term": {
          "command": "alacritty --class scratchpad",
          "margin": 50,
          "unfocus": true
        },
        "ranger": {
          "command": "kitty --class scratchpad -e ranger",
          "margin": 50,
          "unfocus": true
        },
        "musikcube": {
          "command": "alacritty --class scratchpad -e musikcube",
          "margin": 50,
          "unfocus": true
        },
        "btm": {
          "command": "alacritty --class scratchpad -e btm",
          "margin": 50,
          "unfocus": true
        },
        "geary": {
          "command": "geary",
          "margin": 50,
          "unfocus": true
        }
      }
    }
  '';
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      terminal = "${pkgs.alacritty}/bin/alacritty";
    };
    colors.background = config.lib.stylix.colors.base00+"e6";
  };
  services.fnott.enable = true;
  services.fnott.settings = {
    main = {
      anchor = "bottom-right";
      stacking-order = "top-down";
      min-width = 400;
      title-font = font+":size=14";
      summary-font = font+":size=12";
      body-font = font+":size=11";
      border-size = 0;
    };
    low = {
      background = config.lib.stylix.colors.base00+"e6";
      title-color = config.lib.stylix.colors.base03+"ff";
      summary-color = config.lib.stylix.colors.base03+"ff";
      body-color = config.lib.stylix.colors.base03+"ff";
      idle-timeout = 150;
      max-timeout = 30;
      default-timeout = 8;
    };
    normal = {
      background = config.lib.stylix.colors.base00+"e6";
      title-color = config.lib.stylix.colors.base07+"ff";
      summary-color = config.lib.stylix.colors.base07+"ff";
      body-color = config.lib.stylix.colors.base07+"ff";
      idle-timeout = 150;
      max-timeout = 30;
      default-timeout = 8;
    };
    critical = {
      background = config.lib.stylix.colors.base00+"e6";
      title-color = config.lib.stylix.colors.base08+"ff";
      summary-color = config.lib.stylix.colors.base08+"ff";
      body-color = config.lib.stylix.colors.base08+"ff";
      idle-timeout = 0;
      max-timeout = 0;
      default-timeout = 0;
    };
  };
}
