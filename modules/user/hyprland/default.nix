{ config, lib, pkgs, inputs, ... }:
let
  cfg = config.userSettings.hyprland;
  font = config.stylix.fonts.monospace.name;
  term = config.userSettings.terminal;
  spawnEditor = config.userSettings.spawnEditor;
  spawnBrowser = config.userSettings.spawnBrowser;
in
{
  options = {
    userSettings.hyprland = {
      enable = lib.mkEnableOption "Enable hyprland";
    };
  };

  config = lib.mkIf cfg.enable {
    userSettings.alacritty.enable = true;
    userSettings.kitty.enable = true;
    userSettings.dmenuScripts = {
      enable = true;
      dmenuCmd = "fuzzel -d";
    };
    userSettings.hyprland.hyprprofiles.enable = lib.mkDefault true;
    userSettings.stylix.enable = true;

    home.sessionVariables = {
      NIXOS_OZONE_WL = 1;
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      GDK_BACKEND = "wayland,x11,*";
      QT_QPA_PLATFORM = "wayland;xcb";
      QT_QPA_PLATFORMTHEME = lib.mkForce "qt5ct";
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      CLUTTER_BACKEND = "wayland";
      GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
      GSK_RENDERER = "gl";
      XCURSOR_THEME = config.gtk.cursorTheme.name;
    };

    gtk.cursorTheme = {
      package = pkgs.quintom-cursor-theme;
      name = if (config.stylix.polarity == "light") then "Quintom_Ink" else "Quintom_Snow";
      size = 36;
    };

    wayland.windowManager.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      plugins = [ ];
      settings = {
        env = [
          "AQ_DRM_DEVICES,${config.home.sessionVariables.AQ_DRM_DEVICES}"
          "AW_NO_MODIFIERS,1"
        ];
        exec-once = [
          "hyprctl setcursor ${config.gtk.cursorTheme.name} ${builtins.toString config.gtk.cursorTheme.size}"
          "hyprpaper"
          "WGPU_BACKEND=gl ashell"
          "hypridle"
          "iio-hyprland"
          "hyprprofile Default"
          "ydotoold"
          "GOMAXPROCS=1 syncthing --no-browser"
          "protonmail-bridge --noninteractive"
          "sleep 5 && libinput-gestures"
          "obs-notification-mute-daemon"
          "alacritty --class scratch_term"
          "kitty --class scratch_yazi -e yazi"
          "alacritty --class scratch_numbat -e numbat"
          "kitty --class scratch_music -e kew"
          "alacritty --class scratch_btm -e btm"
          "element-desktop"
        ];

        general = {
          layout = "master";
          border_size = 5;
          "col.active_border" = "0xff${config.lib.stylix.colors.base08} 0xff${config.lib.stylix.colors.base09} 0xff${config.lib.stylix.colors.base0A} 0xff${config.lib.stylix.colors.base0B} 0xff${config.lib.stylix.colors.base0C} 0xff${config.lib.stylix.colors.base0D} 0xff${config.lib.stylix.colors.base0E} 0xff${config.lib.stylix.colors.base0F} 270deg";
          "col.inactive_border" = "0xff${config.lib.stylix.colors.base02}";
          resize_on_border = true;
          gaps_in = 7;
          gaps_out = 7;
        };

        group = {
          "col.border_active" = "0xff${config.lib.stylix.colors.base08} 0xff${config.lib.stylix.colors.base09} 0xff${config.lib.stylix.colors.base0A} 0xff${config.lib.stylix.colors.base0B} 0xff${config.lib.stylix.colors.base0C} 0xff${config.lib.stylix.colors.base0D} 0xff${config.lib.stylix.colors.base0E} 0xff${config.lib.stylix.colors.base0F} 270deg";
          "col.border_inactive" = "0xff${config.lib.stylix.colors.base02}";
          groupbar = {
            gradients = false;
            "col.active" = "0xff${config.lib.stylix.colors.base0B}";
            "col.inactive" = "0xff${config.lib.stylix.colors.base02}";
          };
        };

        decoration = {
          shadow = {
            enabled = true;
          };
          rounding = 8;
          dim_special = 0.0;
          blur = {
            enabled = true;
            size = 5;
            passes = 2;
            ignore_opacity = true;
            contrast = 1.17;
            brightness = (if (config.stylix.polarity == "dark") then "0.65" else "1.45");
            xray = true;
            special = true;
            popups = true;
          };
        };

        cursor = {
          no_hardware_cursors = 1;
          no_warps = false;
          inactive_timeout = 30;
        };

        misc = {
          disable_hyprland_logo = true;
          mouse_move_enables_dpms = true;
          enable_swallow = true;
          swallow_regex = "(scratch_term)|(Alacritty)|(kitty)|(scratch_yazi)";
          font_family = font;
          focus_on_activate = true;
        };

        bezier = [
          "wind, 0.05, 0.9, 0.1, 1.05"
          "winIn, 0.1, 1.1, 0.1, 1.0"
          "winOut, 0.3, -0.3, 0, 1"
          "liner, 1, 1, 1, 1"
          "linear, 0.0, 0.0, 1.0, 1.0"
        ];

        animations = {
          enabled = "yes";
          animation = [
            "windowsIn, 1, 6, winIn, popin"
            "windowsOut, 1, 5, winOut, popin"
            "windowsMove, 1, 5, wind, slide"
            "border, 1, 10, default"
            "borderangle, 1, 100, linear, loop"
            "fade, 1, 10, default"
            "workspaces, 1, 5, wind"
            "windows, 1, 6, wind, slide"
            "specialWorkspace, 1, 6, default, slidefadevert -50%"
          ];
        };

        input = {
          kb_layout = "us";
          kb_options = "caps:escape";
          repeat_delay = 450;
          repeat_rate = 50;
          accel_profile = "adaptive";
          follow_mouse = 2;
          float_switch_override_focus = 0;
        };

        binds = {
          movefocus_cycles_fullscreen = false;
        };

        bind = [
          "SUPER,code:9,exec,nwggrid-wrapper"
          "SUPER,code:66,exec,nwggrid-wrapper"
          "SUPER,SPACE,fullscreen,1"
          "SUPERSHIFT,F,fullscreen,0"
          "SUPER,Y,workspaceopt,allfloat"
          "ALT,TAB,cyclenext"
          "ALT,TAB,bringactivetotop"
          "ALTSHIFT,TAB,cyclenext,prev"
          "ALTSHIFT,TAB,bringactivetotop"
          "SUPER,W,togglegroup"
          "SUPER,TAB,changegroupactive,f"
          "SUPERSHIFT,TAB,changegroupactive,b"
          ''SUPER,V,exec,wl-copy $(wl-paste | tr "\n" " ")''
          "SUPERSHIFT,T,exec,screenshot-ocr"
          "CTRLALT,Delete,exec,hyprctl kill"
          "SUPERSHIFT,K,exec,hyprctl kill"
          ",code:172,exec,mpc toggle"
          ",code:208,exec,mpc toggle"
          ",code:209,exec,mpc toggle"
          ",code:174,exec,mpc stop"
          ",code:171,exec,mpc next"
          ",code:173,exec,mpc prev"
          "SUPER,R,pass,^(com\.obsproject\.Studio)$"
          "SUPERSHIFT,R,pass,^(com\.obsproject\.Studio)$"
          "SUPER,RETURN,exec,${term}"
          "SUPERSHIFT,RETURN,exec,${term} --class float_term"
          "SUPER,A,exec,${spawnEditor}"
          "SUPER,S,exec,${spawnBrowser}"
          "SUPERCTRL,S,exec,container-open"
          "SUPERCTRL,P,pin"
          "SUPER,code:47,exec,fuzzel"
          "SUPER,X,exec,fnottctl dismiss"
          "SUPERSHIFT,X,exec,fnottctl dismiss all"
          "SUPER,Q,killactive"
          "SUPERSHIFT,Q,exit"
          "SUPER,T,togglefloating"
          ",code:148,exec,${term} -e numbat"
          '',code:107,exec,grim -g "$(slurp)"''
          ''SHIFT,code:107,exec,grim -g "$(slurp -o)''
          "SUPER,code:107,exec,grim"
          ''CTRL,code:107,exec,grim -g "$(slurp)" - | wl-copy''
          ''SHIFTCTRL,code:107,exec,grim -g "$(slurp -o)" - | wl-copy''
          "SUPERCTRL,code:107,exec,grim - | wl-copy"
          ",code:122,exec,swayosd-client --output-volume lower"
          ",code:123,exec,swayosd-client --output-volume raise"
          ",code:121,exec,swayosd-client --output-volume mute-toggle"
          ",code:256,exec,swayosd-client --output-volume mute-toggle"
          "SHIFT,code:122,exec,swayosd-client --output-volume lower"
          "SHIFT,code:123,exec,swayosd-client --output-volume raise"
          ",code:232,exec,swayosd-client --brightness lower"
          ",code:233,exec,swayosd-client --brightness raise"
          ",code:237,exec,brightnessctl --device='asus::kbd_backlight' set 1-"
          ",code:238,exec,brightnessctl --device='asus::kbd_backlight' set +1"
          ",code:255,exec,airplane-mode"
          "SUPER,C,exec,wl-copy $(hyprpicker)"
          "SUPERSHIFT,S,exec,systemctl suspend"
          "SUPERCTRL,L,exec,loginctl lock-session"
          "SUPERCTRL,G,exec,hyprgamemode"
          "SUPER,H,movefocus,l"
          "SUPER,J,movefocus,d"
          "SUPER,K,movefocus,u"
          "SUPER,L,movefocus,r"
          "SUPERSHIFT,H,movewindow,l"
          "SUPERSHIFT,J,movewindow,d"
          "SUPERSHIFT,K,movewindow,u"
          "SUPERSHIFT,L,movewindow,r"
          "SUPER,1,focusworkspaceoncurrentmonitor,1"
          "SUPER,2,focusworkspaceoncurrentmonitor,2"
          "SUPER,3,focusworkspaceoncurrentmonitor,3"
          "SUPER,4,focusworkspaceoncurrentmonitor,4"
          "SUPER,5,focusworkspaceoncurrentmonitor,5"
          "SUPER,6,focusworkspaceoncurrentmonitor,6"
          "SUPER,7,focusworkspaceoncurrentmonitor,7"
          "SUPER,8,focusworkspaceoncurrentmonitor,8"
          "SUPER,9,focusworkspaceoncurrentmonitor,9"
          "SUPERCTRL,right,exec,hyprnome"
          "SUPERCTRL,left,exec,hyprnome --previous"
          "SUPERSHIFT,right,exec,hyprnome --move"
          "SUPERSHIFT,left,exec,hyprnome --previous --move"
          "SUPERSHIFT,1,movetoworkspace,1"
          "SUPERSHIFT,2,movetoworkspace,2"
          "SUPERSHIFT,3,movetoworkspace,3"
          "SUPERSHIFT,4,movetoworkspace,4"
          "SUPERSHIFT,5,movetoworkspace,5"
          "SUPERSHIFT,6,movetoworkspace,6"
          "SUPERSHIFT,7,movetoworkspace,7"
          "SUPERSHIFT,8,movetoworkspace,8"
          "SUPERSHIFT,9,movetoworkspace,9"
          ''SUPER,Z,exec,if hyprctl clients | grep scratch_term; then echo "scratch_term respawn not needed"; else alacritty --class scratch_term; fi''
          "SUPER,Z,togglespecialworkspace,scratch_term"
          ''SUPER,F,exec,if hyprctl clients | grep scratch_yazi; then echo "scratch_yazi respawn not needed"; else kitty --class scratch_yazi -e yazi; fi''
          "SUPER,F,togglespecialworkspace,scratch_yazi"
          ''SUPER,N,exec,if hyprctl clients | grep scratch_numbat; then echo "scratch_numbat respawn not needed"; else alacritty --class scratch_numbat -e numbat; fi''
          "SUPER,N,togglespecialworkspace,scratch_numbat"
          ''SUPER,M,exec,if hyprctl clients | grep scratch_music; then echo "scratch_music respawn not needed"; else kitty --class scratch_music -e kew; fi''
          "SUPER,M,togglespecialworkspace,scratch_music"
          ''SUPER,B,exec,if hyprctl clients | grep scratch_btm; then echo "scratch_yazi respawn not needed"; else alacritty --class scratch_btm -e btm; fi''
          "SUPER,B,togglespecialworkspace,scratch_btm"
          ''SUPER,D,exec,if hyprctl clients | grep Element; then echo "scratch_yazi respawn not needed"; else element-desktop; fi''
          "SUPER,D,togglespecialworkspace,scratch_element"
          ''SUPER,equal, exec, hyprctl keyword cursor:zoom_factor "$(hyprctl getoption cursor:zoom_factor | grep float | awk '{print $2 + 0.5}')"''
          ''SUPER,minus, exec, hyprctl keyword cursor:zoom_factor "$(hyprctl getoption cursor:zoom_factor | grep float | awk '{print $2 - 0.5}')"''
          "SUPER,I,exec,networkmanager_dmenu"
          "SUPER,P,exec,keepmenu"
          "SUPERSHIFT,P,exec,hyprprofile-dmenu"
          "SUPERCTRL,R,exec,phoenix refresh"
        ];

        bindm = [
          "SUPER,mouse:272,movewindow"
          "SUPER,mouse:273,resizewindow"
        ];

        bindl = [
          ",switch:on:Lid Switch,exec,loginctl lock-session"
        ];

        windowrulev2 = [
          "float,class:^(scratch_term)$"
          "size 80% 85%,class:^(scratch_term)$"
          "workspace special:scratch_term silent ,class:^(scratch_term)$"
          "center,class:^(scratch_term)$"
          "float,class:^(float_term)$"
          "center,class:^(float_term)$"
          "float,class:^(scratch_yazi)$"
          "size 80% 85%,class:^(scratch_yazi)$"
          "workspace special:scratch_yazi silent,class:^(scratch_yazi)$"
          "center,class:^(scratch_yazi)$"
          "float,class:^(filechoose_yazi)$"
          "size 80% 85%,class:^(filechoose_yazi)$"
          "center,class:^(filechoose_yazi)$"
          "float,class:^(scratch_numbat)$"
          "size 80% 85%,class:^(scratch_numbat)$"
          "workspace special:scratch_numbat silent,class:^(scratch_numbat)$"
          "center,class:^(scratch_numbat)$"
          "float,class:^(scratch_btm)$"
          "size 80% 85%,class:^(scratch_btm)$"
          "workspace special:scratch_btm silent,class:^(scratch_btm)$"
          "center,class:^(scratch_btm)$"
          "float,class:^(Element)$"
          "size 85% 90%,class:^(Element)$"
          "workspace special:scratch_element silent,class:^(Element)$"
          "center,class:^(Element)$"
          "float,class:^(scratch_music)$"
          "size 80% 85%,class:^(scratch_music)$"
          "workspace special:scratch_music silent,class:^(scratch_music)$"
          "center,class:^(scratch_music)$"
          "float,title:^(Save to Disk)$"
          "size 70% 75%,title:^(Save to Disk)$"
          "center,title:^(Save to Disk)$"
          "opacity 0.80,class:^(org.pulseaudio.pavucontrol)$"
          "float,class:^(pokefinder)$"
          "float,class:^(Waydroid)$"
          "float,title:(Blender Render)"
          "size 86% 85%,title:(Blender Render)"
          "center,title:(Blender Render)"
          #"float,class:^(org.inkscape.Inkscape)$"
          #"float,class:^(pinta)$"
          #"float,class:^(krita)$"
          #"float,class:^(Gimp)"
          #"float,class:^(Gimp)"
          "float,class:^(libresprite)$"
          "float,title:(Open Images)"
          "size 86% 85%,title:(Open Images)"
          "center,title:(Open Images)"
          "float,title:(Create new document)"
          "size 86% 85%,title:(Create new document)"
          "center,title:(Create new document)"
          "size 86% 85%,title:(Create new document)"
          "float,title:(Create New Node)"
          "size 70% 70%,title:(Create New Node)"
          "center,title:(Create New Node)"
          "float,title:(Resource)"
          "size 70% 70%,title:(Resource)"
          "center,title:(Resource)"
          "tile,title:(Godot)"
          "opacity 0.80,title:ORUI"
          "suppressevent maximize,class:^(steam)$"
          #"float,class:^(steam)$"
          #"fullscreen,class:^(steam)$"
          "opacity 1.0,class:^(org.qutebrowser.qutebrowser),fullscreen:1"
          "opacity 0.85,class:^(Element)$"
          "opacity 0.85,class:^(Logseq)$"
          "opacity 1.0,class:^(Brave-browser),fullscreen:1"
          "opacity 1.0,class:^(librewolf),fullscreen:1"
          "opacity 0.85,title:^(My Local Dashboard Awesome Homepage - qutebrowser)$"
          "opacity 0.85,title:\[.*\] - My Local Dashboard Awesome Homepage"
          "opacity 0.85,class:^(org.keepassxc.KeePassXC)$"
          "opacity 0.85,class:^(org.gnome.Nautilus)$"
          "opacity 0.85,class:^(org.gnome.Nautilus)$"
          "opacity 0.85,initialTitle:^(Notes)$,initialClass:^(Brave-browser)$"
        ];

        layerrule = [
          "blur,waybar"
          "blur,ashell"
          "blur,launcher # fuzzel"
          "blur,~nwggrid"
          "blur,gtk-layer-shell"
          "xray 1,waybar"
          "xray 1,ashell"
          "xray 1,~nwggrid"
          "xray 1,gtk-layer-shell"
          "ignorezero, gtk-layer-shell"
          "ignorezero, ashell"
          "animation fade,~nwggrid"
          "animation popin 80%, ashell"
        ];

        blurls = [
          "waybar"
          "launcher # fuzzel"
          "~nwggrid"
          "gtk-layer-shell"
        ];

        xwayland = {
          force_zero_scaling = true;
        };

        ecosystem = {
          no_update_news = true;
          no_donation_nag = true;
        };

      };
      systemd.variables = ["--all"];
      xwayland = { enable = true; };
      systemd.enable = true;
    };

    home.packages = (with pkgs; [
      hyprland-monitor-attached
      kew
      alacritty
      kitty
      killall
      polkit_gnome
      (ashell.overrideAttrs (o: {
          patches = (o.patches or [ ]) ++ [
            ./ashell.patch
          ];
        }))
      nwg-launchers
      papirus-icon-theme
      (pkgs.writeScriptBin "nwggrid-wrapper" ''
        #!/bin/sh
        if pgrep -x "nwggrid-server" > /dev/null
        then
          nwggrid -client
        else
          GDK_PIXBUF_MODULE_FILE=${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache nwggrid-server -layer-shell-exclusive-zone -1 -g adw-gtk3 -o 0.55 -b ${config.lib.stylix.colors.base00}
        fi
      '')
      (pkgs.writeScriptBin "hyprgamemode" ''
        #!/bin/sh
        HYPRGAMEMODE=$(hyprctl getoption decoration:blur:enabled | awk 'NR==1{print $2}')
        if [ "$HYPRGAMEMODE" = 1 ] ; then
            sync;
            hyprctl --batch "\
                keyword animations:enabled 0;\
                keyword decoration:shadow:enabled 0;\
                keyword decoration:blur:enabled 0;\
                keyword general:gaps_in 0;\
                keyword general:gaps_out 0;\
                keyword general:border_size 1;\
                keyword decoration:rounding 0";
            pkill -STOP electron;
            pkill -STOP syncthing;
            pkill -STOP emacs;
            pkill -STOP emacsclient;
            systemctl --user stop mpd;
            systemctl --user stop nextcloud-client;
            pkill ashell;
            pkill hypridle;
            exit;
        else
            hyprctl --batch "\
                keyword animations:enabled ${builtins.toString config.wayland.windowManager.hyprland.settings.animations.enabled};\
                keyword decoration:shadow:enabled ${builtins.toString config.wayland.windowManager.hyprland.settings.decoration.shadow.enabled};\
                keyword decoration:blur:enabled ${builtins.toString config.wayland.windowManager.hyprland.settings.decoration.blur.enabled};\
                keyword general:gaps_in ${builtins.toString config.wayland.windowManager.hyprland.settings.general.gaps_in};\
                keyword general:gaps_out ${builtins.toString config.wayland.windowManager.hyprland.settings.general.gaps_out};\
                keyword general:border_size ${builtins.toString config.wayland.windowManager.hyprland.settings.general.border_size};\
                keyword decoration:rounding ${builtins.toString config.wayland.windowManager.hyprland.settings.decoration.rounding}";
            pkill -CONT electron;
            pkill -CONT syncthing;
            pkill -CONT emacs;
            pkill -CONT emacsclient;
            systemctl --user start mpd;
            systemctl --user start nextcloud-client;
            WGPU_BACKEND=gl ashell & disown;
            hypridle & disown;
            exit;
        fi
      '')
      libva-utils
      libinput-gestures
      gsettings-desktop-schemas
      (pkgs.makeDesktopItem {
        name = "nwggrid";
        desktopName = "Application Launcher";
        exec = "nwggrid-wrapper";
        terminal = false;
        type = "Application";
        noDisplay = true;
        icon = "${config.home.homeDirectory}/.local/share/pixmaps/hyprland-logo-stylix.svg";
      })
      hyprnome
      wlr-randr
      wtype
      ydotool
      wl-clipboard
      hyprland-protocols
      hyprpicker
      inputs.hyprlock.packages.${pkgs.system}.default
      hypridle
      hyprpaper
      fnott
      keepmenu
      pinentry-gnome3
      wev
      grim
      slurp
      libsForQt5.qt5.qtwayland
      qt6.qtwayland
      xdg-utils
      wlsunset
      pavucontrol
      (pkgs.writeScriptBin "workspace-on-monitor" ''
      #!/bin/sh
      hyprctl monitors -j | jq ".[$1] | .activeWorkspace.id"
      '')
      (pkgs.writeScriptBin "open-under-ranger" ''
      #!/bin/sh
      command="$1"
      echo $command
      file="''${*:2}"
      file=''${file// /\\ }
      echo $file
      workspace=$(hyprctl monitors -j | jq ".[] | select(.specialWorkspace.name == \"special:scratch_yazi\") | .activeWorkspace.id")
      if [ -z "''${workspace}" ]; then
        hyprctl dispatch exec -- "$command";
      else
        hyprctl dispatch exec "[workspace $workspace]" -- "$command" "$file";
      fi
      hyprctl dispatch togglespecialworkspace scratch_yazi
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
            else
              pkill -CONT fnott;
          fi
          sleep 10;
        done
      '')
      (pkgs.writeScriptBin "suspend-unless-render" ''
        #!/bin/sh
        if pgrep -x nixos-rebuild > /dev/null || pgrep -x home-manager > /dev/null || pgrep -x kdenlive > /dev/null || pgrep -x FL64.exe > /dev/null || pgrep -x blender > /dev/null || pgrep -x flatpak > /dev/null;
        then echo "Shouldn't suspend"; sleep 10; else echo "Should suspend"; systemctl suspend; fi
      '')
    ]);
    home.file.".config/ashell.yml".text = ''
outputs: All
position: Top
modules:
  left:
    - [ AppLauncher, SystemInfo ]
  center:
    - Workspaces
  right:
    - [Clock, Settings, Tray]
appLauncherCmd: "nwggrid-wrapper" # optional, default None
truncateTitleAfterLength: 150 # optional, default 150
workspaces:
  visibilityMode: MonitorSpecific # optional, default All
  enableWorkspaceFilling: true # optional, default false
system:
  cpuWarnThreshold: 80 # cpu indicator warning level (default 60)
  cpuAlertThreshold: 95 # cpu indicator alert level (default 80)
  memWarnThreshold: 50 # mem indicator warning level (default 70)
  memAlertThreshold: 75 # mem indicator alert level (default 85)
  tempWarnThreshold: 90 # temperature indicator warning level (default 60)
  tempAlertThreshold: 95 # temperature indicator alert level (default 80)
clock:
  format: "%a %d %b %R" # optional, default: %a %d %b %R
mediaPlayer:
  maxTitleLength: 100 # optional, default 100
settings:
  lockCmd: "hyprlock &" # optional, default None
  audioSinksMoreCmd: "pavucontrol -t 3" # optional default None
  audioSourcesMoreCmd: "pavucontrol -t 4" # optional, default None
  wifiMoreCmd: "nm-connection-editor" # optional, default None
  vpnMoreCmd: "nm-connection-editor" # optional, default None
  bluetoothMoreCmd: "blueman-manager" # optional, default None
appearance:
  backgroundColor: "#${config.lib.stylix.colors.base00}88" # used as a base background color for header module button
  primaryColor: "#${config.lib.stylix.colors.base0B}" # used as a accent color
  secondaryColor: "#${config.lib.stylix.colors.base01}" # used for darker background color
  successColor: "#${config.lib.stylix.colors.base0A}" # used for success message or happy state
  dangerColor: "#${config.lib.stylix.colors.base08}" # used for danger message or danger state (the weak version is used for the warning state
  textColor: "#${config.lib.stylix.colors.base07}" # base default text color
  # this is a list of color that will be used in the workspace module (one color for each monitor)
  workspaceColors:
    - "#${config.lib.stylix.colors.base0B}"
    - "#${config.lib.stylix.colors.base0B}"
  # this is a list of color that will be used in the workspace module
  # for the special workspace (one color for each monitor)
  # optional, default None
  # without a value the workspaceColors list will be used
  specialWorkspaceColors:
    - "#${config.lib.stylix.colors.base0B}"
    - "#${config.lib.stylix.colors.base0B}"
    '';
    home.file.".config/hypr/hypridle.conf".text = ''
      general {
        lock_cmd = pgrep hyprlock || hyprlock
        before_sleep_cmd = loginctl lock-session
        ignore_dbus_inhibit = false
      }

      listener {
        timeout = 165 # in seconds
        on-timeout = loginctl lock-session
      }
      listener {
        timeout = 180 # in seconds
        on-timeout = systemctl suspend
        on-resume = hyprctl dispatch dpms on
      }
    '';
    home.file.".config/hypr/hyprlock.conf".text = ''
      background {
        monitor =
        path = screenshot

        # all these options are taken from hyprland, see https://wiki.hyprland.org/Configuring/Variables/#blur for explanations
        blur_passes = 4
        blur_size = 5
        noise = 0.0117
        contrast = 0.8916
        brightness = 0.8172
        vibrancy = 0.1696
        vibrancy_darkness = 0.0
      }

      input-field {
        monitor =
        size = 200, 50
        outline_thickness = 3
        dots_size = 0.33 # Scale of input-field height, 0.2 - 0.8
        dots_spacing = 0.15 # Scale of dots' absolute size, 0.0 - 1.0
        dots_center = false
        dots_rounding = -1 # -1 default circle, -2 follow input-field rounding
        outer_color = rgb(${config.lib.stylix.colors.base07-rgb-r},${config.lib.stylix.colors.base07-rgb-g},${config.lib.stylix.colors.base07-rgb-b})
        inner_color = rgb(${config.lib.stylix.colors.base00-rgb-r},${config.lib.stylix.colors.base00-rgb-g},${config.lib.stylix.colors.base00-rgb-b})
        font_color = rgb(${config.lib.stylix.colors.base07-rgb-r},${config.lib.stylix.colors.base07-rgb-g},${config.lib.stylix.colors.base07-rgb-b})
        fade_on_empty = true
        fade_timeout = 1000 # Milliseconds before fade_on_empty is triggered.
        placeholder_text = <i>Input Password...</i> # Text rendered in the input box when it's empty.
        hide_input = false
        rounding = -1 # -1 means complete rounding (circle/oval)
        check_color = rgb(${config.lib.stylix.colors.base0A-rgb-r},${config.lib.stylix.colors.base0A-rgb-g},${config.lib.stylix.colors.base0A-rgb-b})
        fail_color = rgb(${config.lib.stylix.colors.base08-rgb-r},${config.lib.stylix.colors.base08-rgb-g},${config.lib.stylix.colors.base08-rgb-b})
        fail_text = <i>$FAIL <b>($ATTEMPTS)</b></i> # can be set to empty
        fail_transition = 300 # transition time in ms between normal outer_color and fail_color
        capslock_color = -1
        numlock_color = -1
        bothlock_color = -1 # when both locks are active. -1 means don't change outer color (same for above)
        invert_numlock = false # change color if numlock is off
        swap_font_color = false # see below

        position = 0, -20
        halign = center
        valign = center
      }

      label {
        monitor =
        text = Screen Locked
        color = rgb(${config.lib.stylix.colors.base07-rgb-r},${config.lib.stylix.colors.base07-rgb-g},${config.lib.stylix.colors.base07-rgb-b})
        font_size = 25
        font_family = ${font}

        rotate = 0 # degrees, counter-clockwise

        position = 0, 160
        halign = center
        valign = center
      }

      label {
        monitor =
        text = $TIME12
        color = rgb(${config.lib.stylix.colors.base07-rgb-r},${config.lib.stylix.colors.base07-rgb-g},${config.lib.stylix.colors.base07-rgb-b})
        font_size = 20
        font_family = ${font}
        rotate = 0 # degrees, counter-clockwise

        position = 0, 80
        halign = center
        valign = center
      }
    '';
    services.swayosd.enable = true;
    services.swayosd.topMargin = 0.5;
    home.file.".config/nwg-launchers/nwggrid/terminal".text = "alacritty -e";

    services.udiskie.enable = true;
    services.udiskie.tray = "never";
    programs.fuzzel.enable = true;
    programs.fuzzel.package = pkgs.fuzzel;
    programs.fuzzel.settings = {
      main = {
        font = font + ":size=20";
        dpi-aware = "no";
        show-actions = "yes";
        terminal = "${pkgs.alacritty}/bin/alacritty";
      };
      colors = {
        background = config.lib.stylix.colors.base00 + "bf";
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
        title-font = font + ":size=14";
        summary-font = font + ":size=12";
        body-font = font + ":size=11";
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
    home.file.".config/hypr/hyprpaper.conf".text = ''
      preload = ${config.stylix.image}
      wallpaper = ,${config.stylix.image}
    '';

  };
}
