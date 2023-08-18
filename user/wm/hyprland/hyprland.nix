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
        cursor_inactive_timeout = 30
        border_size = 4
        col.active_border = 0xff''+config.lib.stylix.colors.base08+''

        col.inactive_border = 0x33''+config.lib.stylix.colors.base00+''

        resize_on_border = true
        gaps_in = 7
        gaps_out = 7
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
      bind=SUPERCTRL,L,exec,swaylock --indicator-radius 200 --screenshots --effect-blur 10x10

      bind=SUPER,H,movefocus,l
      bind=SUPER,J,movefocus,d
      bind=SUPER,K,movefocus,u
      bind=SUPER,L,movefocus,r

      bind=SUPERSHIFT,H,movewindow,l
      bind=SUPERSHIFT,J,movewindow,d
      bind=SUPERSHIFT,K,movewindow,u
      bind=SUPERSHIFT,L,movewindow,r

      bind=SUPER,1,exec,hyprworkspace 1
      bind=SUPER,2,exec,hyprworkspace 2
      bind=SUPER,3,exec,hyprworkspace 3
      bind=SUPER,4,exec,hyprworkspace 4
      bind=SUPER,5,exec,hyprworkspace 5
      bind=SUPER,6,exec,hyprworkspace 6
      bind=SUPER,7,exec,hyprworkspace 7
      bind=SUPER,8,exec,hyprworkspace 8
      bind=SUPER,9,exec,hyprworkspace 9

      bind=SUPERSHIFT,1,movetoworkspace,1
      bind=SUPERSHIFT,2,movetoworkspace,2
      bind=SUPERSHIFT,3,movetoworkspace,3
      bind=SUPERSHIFT,4,movetoworkspace,4
      bind=SUPERSHIFT,5,movetoworkspace,5
      bind=SUPERSHIFT,6,movetoworkspace,6
      bind=SUPERSHIFT,7,movetoworkspace,7
      bind=SUPERSHIFT,8,movetoworkspace,8
      bind=SUPERSHIFT,9,movetoworkspace,9

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

      monitor=eDP-1,1920x1080,1000x1200,1
      monitor=HDMI-A-1,1920x1200,1920x0,1
      monitor=DP-1,1920x1200,0x0,1

      xwayland {
        force_zero_scaling = true
      }

      env = WLR_DRM_DEVICES,/dev/dri/card1:/dev/dri/card0

      input {
        kb_layout = us
        kb_options = caps:escape
        repeat_delay = 350
        repeat_rate = 50
        accel_profile = adaptive
        follow_mouse = 2
      }

      decoration {
        rounding = 8
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
    (pkgs.writeScriptBin "hyprworkspace" ''
      #!/bin/sh
      # from https://github.com/taylor85345/hyprland-dotfiles/blob/master/hypr/scripts/workspace
      monitors=/tmp/hypr/monitors_temp
      hyprctl monitors > $monitors

      if [[ -z $1 ]]; then
        workspace=$(grep -B 5 "focused: no" "$monitors" | awk 'NR==1 {print $3}')
      else
        workspace=$1
      fi

      activemonitor=$(grep -B 11 "focused: yes" "$monitors" | awk 'NR==1 {print $2}')
      passivemonitor=$(grep  -B 6 "($workspace)" "$monitors" | awk 'NR==1 {print $2}')
      #activews=$(grep -A 2 "$activemonitor" "$monitors" | awk 'NR==3 {print $1}' RS='(' FS=')')
      passivews=$(grep -A 6 "Monitor $passivemonitor" "$monitors" | awk 'NR==4 {print $1}' RS='(' FS=')')

      if [[ $workspace -eq $passivews ]] && [[ $activemonitor != "$passivemonitor" ]]; then
        hyprctl dispatch swapactiveworkspaces "$activemonitor" "$passivemonitor" &&
        hyprctl reload &&
        hyprctl dispatch focusmonitor "$activemonitor"
        echo $activemonitor $passivemonitor
      else
        hyprctl dispatch moveworkspacetomonitor "$workspace $activemonitor" && hyprctl dispatch workspace "$workspace"
      fi

      exit 0

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

  programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    });
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 30;
        # width = 1280;
        spacing = 4;

        modules-left = [ ];
        modules-center = [ "wlr/workspaces" ];
        modules-right = [ "idle_inhibitor" "pulseaudio" "network" "cpu" "memory" "backlight" "battery" "clock" "tray"];

        "wlr/workspaces" = {
          "format" = "{icon}";
          "format-icons" = {
            "1" = "";
            "2" = "";
            "3" = "";
            "4" = "";
            "5" = "";
          };
          "persistent_workspaces" = {
             "1" = [];
             "2" = [];
             "3" = [];
             "4" = [];
             "5" = [];
             "6" = [];
             "7" = [];
             "8" = [];
             "9" = [];
           };
        };

        "idle_inhibitor" = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };
        tray = {
          #"icon-size" = 21;
          "spacing" = 10;
        };
        clock = {
          "timezone" = "America/New_York";
          "tooltip-format" = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          "format-alt" = "{:%Y-%m-%d}";
        };
        cpu = {
          "format" = "{usage}% ";
          "tooltip" = false;
        };
        memory = {
          "format" = "{}% ";
        };
        backlight = {
            "format" = "{percent}% {icon}";
            "format-icons" = ["" "" "" "" "" "" "" "" ""];
        };
        battery = {
          "states" = {
            "good" = 95;
            "warning" = 30;
            "critical" = 15;
          };
          "format" = "{capacity}% {icon}";
          "format-charging" = "{capacity}% ";
          "format-plugged" = "{capacity}% ";
          "format-alt" = "{time} {icon}";
          #"format-good" = ""; # An empty format will hide the module
          #"format-full" = "";
          "format-icons" = ["" "" "" "" ""];
        };
        network = {
          "format-wifi" = "{essid} ({signalStrength}%) ";
          "format-ethernet" = "{ipaddr}/{cidr} ";
          "tooltip-format" = "{ifname} via {gwaddr} ";
          "format-linked" = "{ifname} (No IP) ";
          "format-disconnected" = "Disconnected ⚠";
          "format-alt" = "{ifname}: {ipaddr}/{cidr}";
        };
        pulseaudio = {
          "scroll-step" = 1;
          "format" = "{volume}% {icon} {format_source}";
          "format-bluetooth" = "{volume}% {icon} {format_source}";
          "format-bluetooth-muted" = " {icon} {format_source}";
          "format-muted" = " {format_source}";
          "format-source" = "{volume}% ";
          "format-source-muted" = "";
          "format-icons" = {
            "headphone" = "";
            "hands-free" = "";
            "headset" = "";
            "phone" = "";
            "portable" = "";
            "car" = "";
            "default" = ["" "" ""];
          };
          "on-click" = "pavucontrol";
        };
      };
    };
    style = ''
      * {
          /* `otf-font-awesome` is required to be installed for icons */
          font-family: FontAwesome, Roboto, Helvetica, Arial, sans-serif;
          font-size: 13px;
      }

      window#waybar {
          background-color: rgba(43, 48, 59, 0.9);
          color: #ffffff;
          transition-property: background-color;
          transition-duration: .5s;
      }

      window#waybar.hidden {
          opacity: 0.2;
      }

      /*
      window#waybar.empty {
          background-color: transparent;
      }
      window#waybar.solo {
          background-color: #FFFFFF;
      }
      */

      window#waybar.termite {
          background-color: #3F3F3F;
      }

      window#waybar.chromium {
          background-color: #000000;
          border: none;
      }

      button {
          /* Use box-shadow instead of border so the text isn't offset */
          box-shadow: inset 0 -3px transparent;
          /* Avoid rounded borders under each button name */
          border: none;
          border-radius: 2;
      }

      /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
      button:hover {
          background: inherit;
      }

      #workspaces button {
          padding: 0 7px;
          background-color: transparent;
          color: #ffffff;
      }

      #workspaces button:hover {
          background: rgba(0, 0, 0, 0.2);
      }

      #workspaces button.focused {
          background-color: #64727D;
          box-shadow: inset 0 -3px #ffffff;
      }

      #workspaces button.active {
          background-color: #ff0000;
      }

      #workspaces button.visible {
          background-color: #0000ff;
      }

      #workspaces button.urgent {
          background-color: #eb4d4b;
      }

      #mode {
          background-color: #64727D;
          border-bottom: 3px solid #ffffff;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #temperature,
      #backlight,
      #network,
      #pulseaudio,
      #wireplumber,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #scratchpad,
      #mpd {
          padding: 0 10px;
          color: #ffffff;
      }

      #window,
      #workspaces {
          margin: 0 4px;
      }

      /* If workspaces is the leftmost module, omit left margin */
      .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
      }

      /* If workspaces is the rightmost module, omit right margin */
      .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
      }

      #clock {
          background-color: #64727D;
      }

      #battery {
          background-color: #ffffff;
          color: #000000;
      }

      #battery.charging, #battery.plugged {
          color: #ffffff;
          background-color: #26A65B;
      }

      @keyframes blink {
          to {
              background-color: #ffffff;
              color: #000000;
          }
      }

      #battery.critical:not(.charging) {
          background-color: #f53c3c;
          color: #ffffff;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      label:focus {
          background-color: #000000;
      }

      #cpu {
          background-color: #2ecc71;
          color: #000000;
      }

      #memory {
          background-color: #9b59b6;
      }

      #disk {
          background-color: #964B00;
      }

      #backlight {
          background-color: #90b1b1;
      }

      #network {
          background-color: #2980b9;
      }

      #network.disconnected {
          background-color: #f53c3c;
      }

      #pulseaudio {
          background-color: #f1c40f;
          color: #000000;
      }

      #pulseaudio.muted {
          background-color: #90b1b1;
          color: #2a5c45;
      }

      #wireplumber {
          background-color: #fff0f5;
          color: #000000;
      }

      #wireplumber.muted {
          background-color: #f53c3c;
      }

      #custom-media {
          background-color: #66cc99;
          color: #2a5c45;
          min-width: 100px;
      }

      #custom-media.custom-spotify {
          background-color: #66cc99;
      }

      #custom-media.custom-vlc {
          background-color: #ffa000;
      }

      #temperature {
          background-color: #f0932b;
      }

      #temperature.critical {
          background-color: #eb4d4b;
      }

      #tray {
          background-color: #2980b9;
      }

      #tray > .passive {
          -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: #eb4d4b;
      }

      #idle_inhibitor {
          background-color: #2d3436;
      }

      #idle_inhibitor.activated {
          background-color: #ecf0f1;
          color: #2d3436;
      }

      #mpd {
          background-color: #66cc99;
          color: #2a5c45;
      }

      #mpd.disconnected {
          background-color: #f53c3c;
      }

      #mpd.stopped {
          background-color: #90b1b1;
      }

      #mpd.paused {
          background-color: #51a37a;
      }

      #language {
          background: #00b093;
          color: #740864;
          padding: 0 5px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state {
          background: #97e1ad;
          color: #000000;
          padding: 0 0px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state > label {
          padding: 0 5px;
      }

      #keyboard-state > label.locked {
          background: rgba(0, 0, 0, 0.2);
      }

      #scratchpad {
          background: rgba(0, 0, 0, 0.2);
      }

      #scratchpad.empty {
      	background-color: transparent;
      }

    '';
  };
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      font = font+":size=13";
      terminal = "${pkgs.alacritty}/bin/alacritty";
    };
    colors = {
      background = config.lib.stylix.colors.base00+"e6";
      text = config.lib.stylix.colors.base07+"ff";
      match = config.lib.stylix.colors.base05+"ff";
      selection = config.lib.stylix.colors.base08+"ff";
      selection-text = config.lib.stylix.colors.base00+"ff";
      selection-match= config.lib.stylix.colors.base05+"ff";
      border = config.lib.stylix.colors.base08+"ff";
    };
    border = {
      width = 3;
      radius = 7;
    };
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
