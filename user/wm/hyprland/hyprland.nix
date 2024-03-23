{ config, lib, pkgs, userSettings, systemSettings, ... }:

{
  imports = [
    ../../app/terminal/alacritty.nix
    ../../app/terminal/kitty.nix
    (import ../../app/dmenu-scripts/networkmanager-dmenu.nix {
      dmenu_command = "fuzzel -d"; inherit config lib pkgs;
    })
    ../input/nihongo.nix
  ] ++
  (if (systemSettings.profile == "personal") then
    [ (import ./hyprprofiles/hyprprofiles.nix {
        dmenuCmd = "fuzzel -d"; inherit config lib pkgs; })]
  else
    []);

  gtk.cursorTheme = {
    package = pkgs.quintom-cursor-theme;
    name = if (config.stylix.polarity == "light") then "Quintom_Ink" else "Quintom_Snow";
    size = 36;
  };

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = [ ];
    settings = { };
    extraConfig = ''
      exec-once = dbus-update-activation-environment DISPLAY XAUTHORITY WAYLAND_DISPLAY
      exec-once = hyprctl setcursor '' + config.gtk.cursorTheme.name + " " + builtins.toString config.gtk.cursorTheme.size + ''

      exec-once = hyprprofile Default

      exec-once = pypr
      exec-once = ydotoold
      #exec-once = STEAM_FRAME_FORCE_CLOSE=1 steam -silent
      exec-once = nm-applet
      exec-once = blueman-applet
      exec-once = GOMAXPROCS=1 syncthing --no-browser
      exec-once = protonmail-bridge --noninteractive
      exec-once = waybar
      exec-once = emacs --daemon

      exec-once = swayidle -w timeout 90 '${config.programs.swaylock.package}/bin/swaylock -f' timeout 210 'suspend-unless-render' resume '${pkgs.hyprland}/bin/hyprctl dispatch dpms on' before-sleep "${config.programs.swaylock.package}/bin/swaylock -f"
      exec-once = obs-notification-mute-daemon

      exec = ~/.swaybg-stylix

      general {
        layout = master
        cursor_inactive_timeout = 30
        border_size = 4
        no_cursor_warps = false
        col.active_border = 0xff'' + config.lib.stylix.colors.base08 + ''

        col.inactive_border = 0x33'' + config.lib.stylix.colors.base00 + ''

            resize_on_border = true
            gaps_in = 7
            gaps_out = 7
       }

       #plugin {
       #  hyprbars {
       #    bar_height = 0
       #    bar_color = 0xee''+ config.lib.stylix.colors.base00 + ''

       #    col.text = 0xff''+ config.lib.stylix.colors.base05 + ''

       #    bar_text_font = '' + userSettings.font + ''

       #    bar_text_size = 12

       #    buttons {
       #      button_size = 0
       #      col.maximize = 0xff''+ config.lib.stylix.colors.base0A +''

       #      col.close = 0xff''+ config.lib.stylix.colors.base08 +''

       #    }
       #  }
       #}

       bind=SUPER,SPACE,fullscreen,1
       bind=SUPERSHIFT,F,fullscreen,0
       bind=SUPER,Y,workspaceopt,allfloat
       bind=ALT,TAB,cyclenext
       bind=ALT,TAB,bringactivetotop
       bind=ALTSHIFT,TAB,cyclenext,prev
       bind=ALTSHIFT,TAB,bringactivetotop
       bind=SUPER,V,exec,wl-copy $(wl-paste | tr '\n' ' ')
       bind=SUPERSHIFT,T,exec,screenshot-ocr

       bind = SUPER,R,pass,^(com\.obsproject\.Studio)$
       bind = SUPERSHIFT,R,pass,^(com\.obsproject\.Studio)$

       bind=SUPER,RETURN,exec,'' + userSettings.term + ''

       bind=SUPER,A,exec,'' + userSettings.spawnEditor + ''

       bind=SUPER,S,exec,'' + userSettings.browser + ''

       bind=SUPERCTRL,S,exec,container-open # qutebrowser only

       bind=SUPERCTRL,R,exec,killall .waybar-wrapped && waybar & disown

       bind=SUPER,code:47,exec,fuzzel
       bind=SUPER,X,exec,fnottctl dismiss
       bind=SUPERSHIFT,X,exec,fnottctl dismiss all
       bind=SUPER,Q,killactive
       bind=SUPERSHIFT,Q,exit
       bindm=SUPER,mouse:272,movewindow
       bindm=SUPER,mouse:273,resizewindow
       bind=SUPER,T,togglefloating
       bind=SUPER,G,exec,hyprworkspace 9; pegasus-fe;
       bind=,code:148,exec,''+ userSettings.term + " "+''-e numbat

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
       bind=SUPER,C,exec,wl-copy $(hyprpicker)

       bind=SUPERSHIFT,S,exec,swaylock --grace 0 & sleep 1 && systemctl suspend
       bind=SUPERCTRL,L,exec,swaylock --grace 0

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

       bind=SUPER,Z,exec,pypr toggle term && hyprctl dispatch bringactivetotop
       bind=SUPER,F,exec,pypr toggle ranger && hyprctl dispatch bringactivetotop
       bind=SUPER,N,exec,pypr toggle musikcube && hyprctl dispatch bringactivetotop
       bind=SUPER,B,exec,pypr toggle btm && hyprctl dispatch bringactivetotop
       bind=SUPER,E,exec,pypr toggle geary && hyprctl dispatch bringactivetotop
       bind=SUPER,code:172,exec,pypr toggle pavucontrol && hyprctl dispatch bringactivetotop
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

       $pavucontrol = class:^(pavucontrol)$
       windowrulev2 = float,$pavucontrol
       windowrulev2 = size 86% 40%,$pavucontrol
       windowrulev2 = move 50% 6%,$pavucontrol
       windowrulev2 = workspace special silent,$pavucontrol
       windowrulev2 = opacity 0.80,$pavucontrol

       windowrulev2 = float,title:^(Kdenlive)$

       windowrulev2 = float,class:^(pokefinder)$

       windowrulev2 = opacity 0.85,$gearyscratchpad
       windowrulev2 = opacity 0.80,title:ORUI
       windowrulev2 = opacity 0.80,title:Heimdall
       windowrulev2 = opacity 0.80,title:^(LibreWolf)$
       windowrulev2 = opacity 0.80,title:^(New Tab - LibreWolf)$
       windowrulev2 = opacity 0.80,title:^(New Tab - Brave)$
       windowrulev2 = opacity 0.75,title:^(My Local Dashboard Awesome Homepage - qutebrowser)$
       windowrulev2 = opacity 0.75,title:\[.*\] - My Local Dashboard Awesome Homepage
       windowrulev2 = opacity 0.9,class:^(org.keepassxc.KeePassXC)$
       windowrulev2 = opacity 0.75,class:^(org.gnome.Nautilus)$

       layerrule = blur,waybar

       bind=SUPER,code:21,exec,pypr zoom
       bind=SUPER,code:21,exec,hyprctl reload

       bind=SUPERCTRL,right,workspace,+1
       bind=SUPERCTRL,left,workspace,-1

       bind=SUPER,I,exec,networkmanager_dmenu
       bind=SUPER,P,exec,keepmenu
       bind=SUPERSHIFT,P,exec,hyprprofile-dmenu

       # 3 monitor setup
       monitor=eDP-1,1920x1080,1000x1080,1
       monitor=HDMI-A-1,1920x1080,1920x0,1
       monitor=DP-1,1920x1080,0x0,1

       # hdmi tv
       #monitor=eDP-1,1920x1080,1920x0,1
       #monitor=HDMI-A-1,1920x1080,0x0,1

       # hdmi work projector
       #monitor=eDP-1,1920x1080,1920x0,1
       #monitor=HDMI-A-1,1920x1200,0x0,1

       xwayland {
         force_zero_scaling = true
       }

       env = WLR_DRM_DEVICES,/dev/dri/card1:/dev/dri/card0
       env = QT_QPA_PLATFORMTHEME,qt5ct

       input {
         kb_layout = us
         kb_options = caps:escape
         repeat_delay = 350
         repeat_rate = 50
         accel_profile = adaptive
         follow_mouse = 2
       }

       misc {
         disable_hyprland_logo = true
         mouse_move_enables_dpms = false
       }
       decoration {
         rounding = 8
         blur {
           enabled = true
           size = 5
           passes = 2
           ignore_opacity = true
           contrast = 1.17
           brightness = 0.8
         }
       }

    '';
    xwayland = { enable = true; };
    systemd.enable = true;
  };

  home.packages = with pkgs; [
    alacritty
    kitty
    feh
    killall
    polkit_gnome
    libva-utils
    gsettings-desktop-schemas
    gnome.zenity
    wlr-randr
    wtype
    ydotool
    wl-clipboard
    hyprland-protocols
    hyprpicker
    swayidle
    swaybg
    fnott
    fuzzel
    keepmenu
    pinentry-gnome3
    wev
    grim
    slurp
    libsForQt5.qt5.qtwayland
    qt6.qtwayland
    xdg-utils
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
    wlsunset
    pavucontrol
    pamixer
    tesseract4
    (pkgs.writeScriptBin "screenshot-ocr" ''
      #!/bin/sh
      imgname="/tmp/screenshot-ocr-$(date +%Y%m%d%H%M%S).png"
      txtname="/tmp/screenshot-ocr-$(date +%Y%m%d%H%M%S)"
      txtfname=$txtname.txt
      grim -g "$(slurp)" $imgname;
      tesseract $imgname $txtname;
      wl-copy -n < $txtfname
    '')
    (pkgs.writeScriptBin "sct" ''
      #!/bin/sh
      killall wlsunset &> /dev/null;
      if [ $# -eq 1 ]; then
        temphigh=$(( $1 + 1 ))
        templow=$1
        wlsunset -t $templow -T $temphigh &> /dev/null &
      else
        killall wlsunset &> /dev/null;
      fi
    '')
    (pkgs.writeScriptBin "obs-notification-mute-daemon" ''
      #!/bin/sh
      while true; do
        if pgrep -x .obs-wrapped > /dev/null;
          then
            pkill -STOP fnott;
            #emacsclient --eval "(org-yaap-mode 0)";
          else
            pkill -CONT fnott;
            #emacsclient --eval "(if (not org-yaap-mode) (org-yaap-mode 1))";
        fi
        sleep 10;
      done
    '')
    (pkgs.writeScriptBin "suspend-unless-render" ''
      #!/bin/sh
      if pgrep -x nixos-rebuild > /dev/null || pgrep -x home-manager > /dev/null || pgrep -x kdenlive > /dev/null || pgrep -x FL64.exe > /dev/null || pgrep -x blender > /dev/null || pgrep -x flatpak > /dev/null;
      then echo "Shouldn't suspend"; sleep 10; else echo "Should suspend"; systemctl suspend; fi
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
      passivews=$(grep -A 6 "Monitor $passivemonitor" "$monitors" | awk 'NR==3 {print $1}' RS='(' FS=')')

      if [[ $workspace -eq $passivews ]] && [[ $activemonitor != "$passivemonitor" ]]; then
       hyprctl dispatch workspace "$workspace" && hyprctl dispatch swapactiveworkspaces "$activemonitor" "$passivemonitor" && hyprctl dispatch workspace "$workspace"
        echo $activemonitor $passivemonitor
      else
        hyprctl dispatch moveworkspacetomonitor "$workspace $activemonitor" && hyprctl dispatch workspace "$workspace"
      fi

      exit 0

    '')
    (pkgs.python3Packages.buildPythonPackage rec {
      pname = "pyprland";
      version = "1.4.1";
      src = pkgs.fetchPypi {
        inherit pname version;
        sha256 = "sha256-JRxUn4uibkl9tyOe68YuHuJKwtJS//Pmi16el5gL9n8=";
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
          "margin": 50
        },
        "ranger": {
          "command": "kitty --class scratchpad -e ranger",
          "margin": 50
        },
        "musikcube": {
          "command": "alacritty --class scratchpad -e musikcube",
          "margin": 50
        },
        "btm": {
          "command": "alacritty --class scratchpad -e btm",
          "margin": 50
        },
        "geary": {
          "command": "geary",
          "margin": 50
        },
        "pavucontrol": {
          "command": "pavucontrol",
          "margin": 50,
          "unfocus": "hide",
          "animation": "fromTop"
        }
      }
    }
  '';

  programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oldAttrs: {
      postPatch = ''
        # use hyprctl to switch workspaces
        sed -i 's/zext_workspace_handle_v1_activate(workspace_handle_);/const std::string command = "hyprworkspace " + name_;\n\tsystem(command.c_str());/g' src/modules/wlr/workspace_manager.cpp
        sed -i 's/gIPC->getSocket1Reply("dispatch workspace " + std::to_string(id()));/const std::string command = "hyprworkspace " + std::to_string(id());\n\tsystem(command.c_str());/g' src/modules/hyprland/workspaces.cpp
      '';
    });
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 35;
        margin = "7 7 3 7";
        spacing = 2;

        modules-left = [ "custom/os" "custom/hyprprofile" "battery" "backlight" "keyboard-state" "pulseaudio" "cpu" "memory" ];
        modules-center = [ "hyprland/workspaces" ];
        modules-right = [ "idle_inhibitor" "tray" "clock" ];

        "custom/os" = {
          "format" = " {} ";
          "exec" = ''echo "" '';
          "interval" = "once";
        };
        "custom/hyprprofile" = {
          "format" = "   {}";
          "exec" = ''cat ~/.hyprprofile'';
          "interval" = 3;
          "on-click" = "hyprprofile-dmenu";
        };
        "keyboard-state" = {
          "numlock" = true;
          "format" = " {icon} ";
          "format-icons" = {
            "locked" = "󰎠";
            "unlocked" = "󱧓";
          };
        };
        "hyprland/workspaces" = {
          "format" = "{icon}";
          "format-icons" = {
            "1" = "󱚌";
            "2" = "󰖟";
            "3" = "";
            "4" = "󰎄";
            "5" = "󰋩";
            "6" = "";
            "7" = "󰄖";
            "8" = "󰑴";
            "9" = "󱎓";
            "scratch_term" = "_";
            "scratch_ranger" = "_󰴉";
            "scratch_musikcube" = "_";
            "scratch_btm" = "_";
            "scratch_geary" = "_";
            "scratch_pavucontrol" = "_󰍰";
          };
          "on-click" = "activate";
          "on-scroll-up" = "hyprctl dispatch workspace e+1";
          "on-scroll-down" = "hyprctl dispatch workspace e-1";
          #"all-outputs" = true;
          #"active-only" = true;
          "ignore-workspaces" = ["scratch" "-"];
          #"show-special" = false;
          #"persistent-workspaces" = {
          #    # this block doesn't seem to work for whatever reason
          #    "eDP-1" = [1 2 3 4 5 6 7 8 9];
          #    "DP-1" = [1 2 3 4 5 6 7 8 9];
          #    "HDMI-A-1" = [1 2 3 4 5 6 7 8 9];
          #    "1" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "2" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "3" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "4" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "5" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "6" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "7" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "8" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #    "9" = ["eDP-1" "DP-1" "HDMI-A-1"];
          #};
        };

        "idle_inhibitor" = {
          format = "{icon}";
          format-icons = {
            activated = "󰅶";
            deactivated = "󰾪";
          };
        };
        tray = {
          #"icon-size" = 21;
          "spacing" = 10;
        };
        clock = {
          "interval" = 1;
          "format" = "{:%a %Y-%m-%d %I:%M:%S %p}";
          "timezone" = "America/Chicago";
          "tooltip-format" = ''
            <big>{:%Y %B}</big>
            <tt><small>{calendar}</small></tt>'';
        };
        cpu = {
          "format" = "{usage}% ";
        };
        memory = { "format" = "{}% "; };
        backlight = {
          "format" = "{percent}% {icon}";
          "format-icons" = [ "" "" "" "" "" "" "" "" "" ];
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
          #"format-good" = ""; # An empty format will hide the module
          #"format-full" = "";
          "format-icons" = [ "" "" "" "" "" ];
        };
        pulseaudio = {
          "scroll-step" = 1;
          "format" = "{volume}% {icon}  {format_source}";
          "format-bluetooth" = "{volume}% {icon}  {format_source}";
          "format-bluetooth-muted" = "󰸈 {icon}  {format_source}";
          "format-muted" = "󰸈 {format_source}";
          "format-source" = "{volume}% ";
          "format-source-muted" = " ";
          "format-icons" = {
            "headphone" = "";
            "hands-free" = "";
            "headset" = "";
            "phone" = "";
            "portable" = "";
            "car" = "";
            "default" = [ "" "" "" ];
          };
          "on-click" = "pypr toggle pavucontrol && hyprctl dispatch bringactivetotop";
        };
      };
    };
    style = ''
      * {
          /* `otf-font-awesome` is required to be installed for icons */
          font-family: FontAwesome, ''+userSettings.font+'';

          font-size: 20px;
      }

      window#waybar {
          background-color: #'' + config.lib.stylix.colors.base00 + '';
          opacity: 0.75;
          border-radius: 8px;
          color: #'' + config.lib.stylix.colors.base07 + '';
          transition-property: background-color;
          transition-duration: .2s;
      }

      window > box {
          border-radius: 8px;
          opacity: 0.94;
      }

      window#waybar.hidden {
          opacity: 0.2;
      }

      button {
          border: none;
      }

      #custom-hyprprofile {
          color: #'' + config.lib.stylix.colors.base0D + '';
      }

      /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
      button:hover {
          background: inherit;
      }

      #workspaces button {
          padding: 0 7px;
          background-color: transparent;
          color: #'' + config.lib.stylix.colors.base04 + '';
      }

      #workspaces button:hover {
          color: #'' + config.lib.stylix.colors.base07 + '';
      }

      #workspaces button.active {
          color: #'' + config.lib.stylix.colors.base08 + '';
      }

      #workspaces button.focused {
          color: #'' + config.lib.stylix.colors.base0A + '';
      }

      #workspaces button.visible {
          color: #'' + config.lib.stylix.colors.base05 + '';
      }

      #workspaces button.urgent {
          color: #'' + config.lib.stylix.colors.base09 + '';
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
          color: #'' + config.lib.stylix.colors.base07 + '';
          border: none;
          border-radius: 8px;
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
          color: #'' + config.lib.stylix.colors.base0D + '';
      }

      #battery {
          color: #'' + config.lib.stylix.colors.base0B + '';
      }

      #battery.charging, #battery.plugged {
          color: #'' + config.lib.stylix.colors.base0C + '';
      }

      @keyframes blink {
          to {
              background-color: #'' + config.lib.stylix.colors.base07 + '';
              color: #'' + config.lib.stylix.colors.base00 + '';
          }
      }

      #battery.critical:not(.charging) {
          background-color: #'' + config.lib.stylix.colors.base08 + '';
          color: #'' + config.lib.stylix.colors.base07 + '';
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      label:focus {
          background-color: #'' + config.lib.stylix.colors.base00 + '';
      }

      #cpu {
          color: #'' + config.lib.stylix.colors.base0D + '';
      }

      #memory {
          color: #'' + config.lib.stylix.colors.base0E + '';
      }

      #disk {
          color: #'' + config.lib.stylix.colors.base0F + '';
      }

      #backlight {
          color: #'' + config.lib.stylix.colors.base0A + '';
      }

      label.numlock {
          color: #'' + config.lib.stylix.colors.base04 + '';
      }

      label.numlock.locked {
          color: #'' + config.lib.stylix.colors.base0F + '';
      }

      #pulseaudio {
          color: #'' + config.lib.stylix.colors.base0C + '';
      }

      #pulseaudio.muted {
          color: #'' + config.lib.stylix.colors.base04 + '';
      }

      #tray > .passive {
          -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
      }

      #idle_inhibitor {
          color: #'' + config.lib.stylix.colors.base04 + '';
      }

      #idle_inhibitor.activated {
          color: #'' + config.lib.stylix.colors.base0F + '';
      }
      '';
  };
  home.file.".config/gtklock/style.css".text = ''
    window {
      background-image: url("''+config.stylix.image+''");
      background-size: auto 100%;
    }
  '';

  services.udiskie.enable = true;
  services.udiskie.tray = "always";
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      color = "#"+config.lib.stylix.colors.base00;
      inside-color = "#"+config.lib.stylix.colors.base00+"cc";
      inside-caps-lock-color = "#"+config.lib.stylix.colors.base09;
      inside-clear-color = "#"+config.lib.stylix.colors.base0A;
      inside-wrong-color = "#"+config.lib.stylix.colors.base08;
      inside-ver-color = "#"+config.lib.stylix.colors.base0D;
      line-color = "#"+config.lib.stylix.colors.base00;
      line-caps-lock-color = "#"+config.lib.stylix.colors.base00;
      line-clear-color = "#"+config.lib.stylix.colors.base00;
      line-wrong-color = "#"+config.lib.stylix.colors.base00;
      line-ver-color = "#"+config.lib.stylix.colors.base00;
      ring-color = "#"+config.lib.stylix.colors.base00;
      ring-caps-lock-color = "#"+config.lib.stylix.colors.base09;
      ring-clear-color = "#"+config.lib.stylix.colors.base0A;
      ring-wrong-color = "#"+config.lib.stylix.colors.base08;
      ring-ver-color = "#"+config.lib.stylix.colors.base0D;
      text-color = "#"+config.lib.stylix.colors.base00;
      key-hl-color = "#"+config.lib.stylix.colors.base0B;
      font = config.stylix.fonts.monospace.name;
      font-size = 20;
      fade-in = 0.5;
      grace = 5;
      indicator-radius = 100;
      screenshots = true;
      effect-blur = "10x10";
    };
  };
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      font = userSettings.font + ":size=13";
      terminal = "${pkgs.alacritty}/bin/alacritty";
    };
    colors = {
      background = config.lib.stylix.colors.base00 + "e6";
      text = config.lib.stylix.colors.base07 + "ff";
      match = config.lib.stylix.colors.base05 + "ff";
      selection = config.lib.stylix.colors.base08 + "ff";
      selection-text = config.lib.stylix.colors.base00 + "ff";
      selection-match = config.lib.stylix.colors.base05 + "ff";
      border = config.lib.stylix.colors.base08 + "ff";
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
      title-font = userSettings.font + ":size=14";
      summary-font = userSettings.font + ":size=12";
      body-font = userSettings.font + ":size=11";
      border-size = 0;
    };
    low = {
      background = config.lib.stylix.colors.base00 + "e6";
      title-color = config.lib.stylix.colors.base03 + "ff";
      summary-color = config.lib.stylix.colors.base03 + "ff";
      body-color = config.lib.stylix.colors.base03 + "ff";
      idle-timeout = 150;
      max-timeout = 30;
      default-timeout = 8;
    };
    normal = {
      background = config.lib.stylix.colors.base00 + "e6";
      title-color = config.lib.stylix.colors.base07 + "ff";
      summary-color = config.lib.stylix.colors.base07 + "ff";
      body-color = config.lib.stylix.colors.base07 + "ff";
      idle-timeout = 150;
      max-timeout = 30;
      default-timeout = 8;
    };
    critical = {
      background = config.lib.stylix.colors.base00 + "e6";
      title-color = config.lib.stylix.colors.base08 + "ff";
      summary-color = config.lib.stylix.colors.base08 + "ff";
      body-color = config.lib.stylix.colors.base08 + "ff";
      idle-timeout = 0;
      max-timeout = 0;
      default-timeout = 0;
    };
  };
}
