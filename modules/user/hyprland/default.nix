{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.userSettings.hyprland;
  font = config.stylix.fonts.monospace.name;
  term = config.userSettings.terminal;
  spawnEditor = config.userSettings.spawnEditor;
  spawnBrowser = config.userSettings.spawnBrowser;
  performance = config.userSettings.hyprland.performanceOptimizations;
in
{
  options = {
    userSettings.hyprland = {
      enable = lib.mkEnableOption "Enable hyprland";
      performanceOptimizations = lib.mkOption {
        default = false;
        type = lib.types.bool;
        description = "Enable performance optimizations";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    userSettings.alacritty.enable = true;
    programs.alacritty.settings.window.opacity = lib.mkOverride 40 (if performance then 1.0 else 0.80);
    userSettings.kitty.enable = true;
    programs.kitty.settings.background_opacity = lib.mkOverride 40 (
      if performance then "1.0" else "0.80"
    );
    userSettings.emacs.opacity = lib.mkOverride 40 (if performance then 100 else 80);
    userSettings.dmenuScripts = {
      enable = true;
      dmenuCmd = "fuzzel -d";
    };
    userSettings.hyprland.hyprprofiles.enable = lib.mkDefault true;
    userSettings.stylix.enable = true;

    home.sessionVariables = {
      NIXOS_OZONE_WL = 1;
      ELECTRON_OZONE_PLATFORM_HINT = "wayland";
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      GDK_BACKEND = "wayland,x11,*";
      QT_QPA_PLATFORM = "wayland;xcb";
      #QT_QPA_PLATFORMTHEME = lib.mkForce "qt5ct";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1.25";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      CLUTTER_BACKEND = "wayland";
      #GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
      #GSK_RENDERER = "gl";
      XCURSOR_THEME = config.gtk.cursorTheme.name;
      GDK_DEBUG = "portals";
      GTK_USE_PORTALS = 1;
      GRIM_DEFAULT_DIR = config.xdg.userDirs.extraConfig.XDG_SCREENSHOT_DIR;
    };

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-termfilechooser
      ];
    };

    xdg.portal.config.common = {
      default = [ "hyprland" ];
      "org.freedesktop.impl.portal.FileChooser" = "termfilechooser";
    };
    xdg.portal.config.hyprland = {
      default = [ "hyprland" ];
      "org.freedesktop.impl.portal.FileChooser" = "termfilechooser";
    };

    home.sessionVariables.TERMCMD = "kitty --class=filechoose_yazi";

    xdg.configFile."xdg-desktop-portal-termfilechooser/config" = {
      force = true;
      text = ''
        [filechooser]
        cmd=${pkgs.xdg-desktop-portal-termfilechooser}/share/xdg-desktop-portal-termfilechooser/yazi-wrapper.sh
      '';
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
          "blueman-applet"
          "kitty --class scratch_yazi -e yazi"
          "alacritty --class scratch_numbat -e numbat"
          "alacritty --class scratch_btm -e btm"
          "element-desktop"
          "sleep 5 && hyprctl keyword bind SUPER,S,exec,${spawnBrowser}"
        ];

        general = {
          layout = "master";
          border_size = 0;
          "col.active_border" =
            if performance then
              "0xff${config.lib.stylix.colors.base0B}"
            else
              "0xff${config.lib.stylix.colors.base08} 0xff${config.lib.stylix.colors.base09} 0xff${config.lib.stylix.colors.base0A} 0xff${config.lib.stylix.colors.base0B} 0xff${config.lib.stylix.colors.base0C} 0xff${config.lib.stylix.colors.base0D} 0xff${config.lib.stylix.colors.base0E} 0xff${config.lib.stylix.colors.base0F} 270deg";
          "col.inactive_border" = "0xff${config.lib.stylix.colors.base02}";
          resize_on_border = true;
          gaps_in = 14;
          gaps_out = 14;
        };

        group = {
          "col.border_active" = config.wayland.windowManager.hyprland.settings.general."col.active_border";
          "col.border_inactive" =
            config.wayland.windowManager.hyprland.settings.general."col.inactive_border";
          groupbar = {
            gradients = false;
            "col.active" = "0xff${config.lib.stylix.colors.base0B}";
            "col.inactive" = "0xff${config.lib.stylix.colors.base02}";
          };
        };

        decoration = {
          shadow = {
            enabled = (!performance);
          };
          rounding = 0;
          dim_special = 0.0;
          dim_inactive = true;
          dim_strength = 0.15;
          blur = {
            enabled = (!performance);
            size = 10;
            passes = 3;
            ignore_opacity = true;
            contrast = 1.17;
            brightness = (if (config.stylix.polarity == "dark") then "0.65" else "1.45");
            xray = (!performance);
            special = (!performance);
            popups = (!performance);
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
          swallow_regex = "(scratch_term)|(Alacritty)|(kitty)";
          font_family = font;
          focus_on_activate = true;
        };

        bezier = lib.optionals (!performance) [
          "wind, 0.05, 0.9, 0.1, 1.05"
          "winIn, 0.1, 1.1, 0.1, 1.0"
          "winOut, 0.3, -0.3, 0, 1"
          "liner, 1, 1, 1, 1"
          "linear, 0.0, 0.0, 1.0, 1.0"
        ];

        animations = {
          enabled = (!performance);
          animation = lib.optionals (!performance) [
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
          "SUPER,R,pass,^(com\.obsproject\.Studio)$"
          "SUPERSHIFT,R,pass,^(com\.obsproject\.Studio)$"
          "SUPER,RETURN,exec,${term}"
          "SUPERSHIFT,RETURN,exec,${term} --class float_term"
          "SUPER,A,exec,${spawnEditor}"
          "SUPERCTRL,S,exec,container-open"
          "SUPERCTRL,P,pin"
          "SUPER,code:47,exec,fuzzel"
          "SUPER,Q,killactive"
          "SUPERSHIFT,Q,exit"
          "SUPER,T,togglefloating"
          ",code:148,exec,${term} -e numbat"
          '',code:107,exec,grim -g "$(slurp)"''
          ''SHIFT,code:107,exec,grim -g "$(slurp -o)"''
          "SUPER,code:107,exec,grim"
          ''CTRL,code:107,exec,grim -g "$(slurp)" - | wl-copy''
          ''SHIFTCTRL,code:107,exec,grim -g "$(slurp -o)" - | wl-copy''
          "SUPERCTRL,code:107,exec,grim - | wl-copy"
          "SUPER,C,exec,wl-copy $(hyprpicker)"
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
          ''SUPER,B,exec,if hyprctl clients | grep scratch_btm; then echo "scratch_yazi respawn not needed"; else alacritty --class scratch_btm -e btm; fi''
          "SUPER,B,togglespecialworkspace,scratch_btm"
          ''SUPER,D,exec,if hyprctl clients | grep Element; then echo "scratch_chat respawn not needed"; else element-desktop; fi''
          "SUPER,D,togglespecialworkspace,scratch_chat"
          ''SUPER,equal, exec, hyprctl keyword cursor:zoom_factor "$(hyprctl getoption cursor:zoom_factor | grep float | awk '{print $2 + 0.5}')"''
          ''SUPER,minus, exec, hyprctl keyword cursor:zoom_factor "$(hyprctl getoption cursor:zoom_factor | grep float | awk '{print $2 - 0.5}')"''
          "SUPER,I,exec,networkmanager_dmenu"
          "SUPER,P,exec,keepmenu"
          "SUPERSHIFT,P,exec,hyprprofile-dmenu"
          "SUPERCTRL,R,exec,phoenix refresh"
          "SUPER,S,exec,${spawnBrowser}"
        ];

        bindr = [
          "SUPER,SUPER_L,exec,nwggrid-wrapper"
        ];

        bindm = [
          "SUPER,mouse:272,movewindow"
          "SUPER,mouse:273,resizewindow"
        ];

        bindl = [
          ",switch:on:Lid Switch,exec,loginctl lock-session"
          "SUPERSHIFT,S,exec,systemctl suspend"
          "SUPERCTRL,L,exec,loginctl lock-session"
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
          "SUPER,X,exec,fnottctl dismiss"
          "SUPERSHIFT,X,exec,fnottctl dismiss all"
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
          "workspace special:scratch_chat silent,class:^(Element)$"
          "center,class:^(Element)$"
          "float,class:^(discord)$"
          "size 85% 90%,class:^(discord)$"
          "workspace special:scratch_chat silent,class:^(discord)$"
          "center,class:^(discord)$"
          "float,title:^(Save to Disk)$"
          "size 70% 75%,title:^(Save to Disk)$"
          "center,title:^(Save to Disk)$"
          "float,class:^(pokefinder)$"
          "float,class:^(Waydroid)$"
          "float,title:(Blender Render)"
          "size 86% 85%,title:(Blender Render)"
          "center,title:(Blender Render)"
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
          "float,title:^(Unlock Database - KeePassXC)$"
          "size 80% 85%,title:^(Unlock Database - KeePassXC)$"
          "center,title:^(Unlock Database - KeepassXC)$"
          "fullscreen,title:^(Steam Big Picture Mode)$"
          "fullscreen,class:^(xfreerdp)$"
        ]
        ++ lib.optionals (!performance) [
          "opacity 0.80,class:^(dev.zed.Zed)$"
          "opacity 0.80,class:^(org.pulseaudio.pavucontrol)$"
          "opacity 1.0,class:^(org.qutebrowser.qutebrowser),fullscreen:1"
          "opacity 0.80,class:^(Element)$"
          "opacity 0.80,class:^(discord)$"
          "opacity 0.80,class:^(Logseq)$"
          "opacity 1.0,class:^(Brave-browser),fullscreen:1"
          "opacity 1.0,class:^(librewolf),fullscreen:1"
          "opacity 0.80,title:^(My Local Dashboard Awesome Homepage - qutebrowser)$"
          "opacity 0.80,title:\[.*\] - My Local Dashboard Awesome Homepage"
          "opacity 0.80,class:^(org.keepassxc.KeePassXC)$"
          "opacity 0.80,class:^(org.gnome.Nautilus)$"
          "opacity 0.80,class:^(org.gnome.Nautilus)$"
          "opacity 0.80,initialTitle:^(Notes)$,initialClass:^(Brave-browser)$"

        ];

        layerrule = lib.optionals (!performance) [
          "blur,waybar"
          "blur,ashell-main-layer"
          "blur,launcher # fuzzel"
          "blur,~nwggrid"
          "blur,notifications"
          "blur,gtk-layer-shell"
          "xray 1,waybar"
          "xray 1,ashell-main-layer"
          "xray 1,~nwggrid"
          "xray 1,gtk-layer-shell"
          "xray 1,notifications"
          "ignorezero, gtk-layer-shell"
          "ignorezero, ashell-main-layer"
          "ignorezero,notifications"
          "animation fade,~nwggrid"
          "animation popin 80%, ashell-main-layer"
          "animation popin 80%, notifications"
          "order 66,ashell-main-layer"
          "order 67,notifications"
          "abovelock 2,ashell-main-layer"
          "abovelock 2,notifications"
        ];

        blurls = lib.optionals (!performance) [
          "waybar"
          "launcher # fuzzel"
          "~nwggrid"
          "gtk-layer-shell"
          "ashell-main-layer"
          "notifications"
        ];

        xwayland = {
          force_zero_scaling = true;
        };

        ecosystem = {
          no_update_news = true;
          no_donation_nag = true;
        };

      };
      systemd.variables = [ "--all" ];
      xwayland = {
        enable = true;
      };
      systemd.enable = true;
    };

    home.packages = (
      with pkgs;
      [
        qpwgraph
        networkmanagerapplet
        hyprland-monitor-attached
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
        (lib.hiPrio papirus-icon-theme)
        (pkgs.writeScriptBin "nwggrid-wrapper" ''
          #!/bin/sh
          if pgrep -x "nwggrid-server" > /dev/null
          then
            nwggrid -client
          else
            GDK_PIXBUF_MODULE_FILE=${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache nwggrid-server -layer-shell-exclusive-zone -1 -g adw-gtk3 -o 0.55 -b ${config.lib.stylix.colors.base00} &
            sleep 0.6 && nwggrid -client
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
                  keyword general:border_size 0;\
                  keyword decoration:rounding 0";
              #pkill -STOP electron;
              #pkill -STOP syncthing;
              #pkill -STOP emacs;
              #pkill -STOP emacsclient;
              #systemctl --user stop nextcloud-client;
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
              #pkill -CONT electron;
              #pkill -CONT syncthing;
              #pkill -CONT emacs;
              #pkill -CONT emacsclient;
              #systemctl --user start nextcloud-client;
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
        kdePackages.qtwayland
        xdg-utils
        wlsunset
        hyprshade
        pavucontrol
        (pkgs.writeScriptBin "workspace-on-monitor" ''
          #!/bin/sh
          hyprctl monitors -j | jq ".[$1] | .activeWorkspace.id"
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
        (pkgs.writeScriptBin "scg" ''
          #!/bin/sh
          hyprshade toggle grayscale;
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
      ]
    );
    home.file.".config/hypr/shaders/grayscale.glsl".text = ''
      /*
       * Grayscale
       */
      #version 300 es

      precision highp float;
      in vec2 v_texcoord;
      uniform sampler2D tex;
      out vec4 fragColor;

      // Enum for type of grayscale conversion
      const int LUMINOSITY = 0;
      const int LIGHTNESS = 1;
      const int AVERAGE = 2;

      /**
       * Type of grayscale conversion.
       */
      const int Type = LUMINOSITY;

      // Enum for selecting luma coefficients
      const int PAL = 0;
      const int HDTV = 1;
      const int HDR = 2;

      /**
       * Formula used to calculate relative luminance.
       * (Only applies to type = "luminosity".)
       */
      const int LuminosityType = HDR;

      void main() {
          vec4 pixColor = texture2D(tex, v_texcoord);

          float gray;
          if (Type == LUMINOSITY) {
              // https://en.wikipedia.org/wiki/Grayscale#Luma_coding_in_video_systems
              if (LuminosityType == PAL) {
                  gray = dot(pixColor.rgb, vec3(0.299, 0.587, 0.114));
              } else if (LuminosityType == HDTV) {
                  gray = dot(pixColor.rgb, vec3(0.2126, 0.7152, 0.0722));
              } else if (LuminosityType == HDR) {
                  gray = dot(pixColor.rgb, vec3(0.2627, 0.6780, 0.0593));
              }
          } else if (Type == LIGHTNESS) {
              float maxPixColor = max(pixColor.r, max(pixColor.g, pixColor.b));
              float minPixColor = min(pixColor.r, min(pixColor.g, pixColor.b));
              gray = (maxPixColor + minPixColor) / 2.0;
          } else if (Type == AVERAGE) {
              gray = (pixColor.r + pixColor.g + pixColor.b) / 3.0;
          }
          vec3 grayscale = vec3(gray);

          fragColor = vec4(grayscale, pixColor.a);
      }
    '';
    home.file.".config/ashell/config.toml".text = ''
      outputs = "All"
      position = "Top"
      app_launcher_cmd = "nwggrid-wrapper"
      truncate_title_after_length = 150
      [modules]
      left = [ "AppLauncher", "SystemInfo" ]
      center = [ "Workspaces" ]
      right = [ "Clock", "Settings", "Tray" ]
      [workspaces]
      visibility_mode = "MonitorSpecific"
      enable_workspace_filling = true
      [system.cpu]
      warn_threshold = 80
      alert_threshold = 95
      [system.mem]
      warn_threshold = 50
      alert_threshold = 75
      [system.temp]
      warn_threshold = 85
      alert_threshold = 95
      [clock]
      format = "%a %d %b %H:%M:%S"
      [mediaPlayer]
      max_title_length = 100
      [settings]
      lockCmd = "hyprlock &"
      audio_sinks_more_cmd = "pavucontrol -t 3"
      audio_sources_more_cmd = "pavucontrol -t 4"
      wifi_more_cmd = "nm-connection-editor"
      vpn_more_cmd = "nm-connection-editor"
      bluetooth_more_cmd = "blueman-manager"
      [appearance]
      scale_factor = 1.25
      style = "Solid"
      opacity = ${if performance then "1.0" else "0.7"}
      background_color = "#${config.lib.stylix.colors.base00}88"
      primary_color = "#${config.lib.stylix.colors.base0A}"
      secondary_color = "#${config.lib.stylix.colors.base01}"
      success_color = "#${config.lib.stylix.colors.base0A}"
      danger_color = "#${config.lib.stylix.colors.base08}"
      text_color = "#${config.lib.stylix.colors.base07}"
      workspace_colors = [ "#${config.lib.stylix.colors.base0B}", "#${config.lib.stylix.colors.base0B}" ]
      specialWorkspaceColors = [ "#${config.lib.stylix.colors.base0B}", "#${config.lib.stylix.colors.base0B}" ]
      [appearance.menu]
      opacity = ${if performance then "1.0" else "0.7"}
      backdrop = 0.0
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
    services.hyprpolkitagent.enable = true;
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
        background = config.lib.stylix.colors.base00 + (if performance then "ff" else "bf");
        text = config.lib.stylix.colors.base07 + "ff";
        match = config.lib.stylix.colors.base05 + "ff";
        selection = config.lib.stylix.colors.base08 + "ff";
        selection-text = config.lib.stylix.colors.base00 + "ff";
        selection-match = config.lib.stylix.colors.base05 + "ff";
        border = config.lib.stylix.colors.base08 + "ff";
      };
      border = {
        width = 0;
        radius = 0;
      };
    };
    services.fnott.enable = true;
    services.fnott.settings = {
      main = {
        anchor = "bottom-right";
        stacking-order = "top-down";
        min-width = 520;
        title-font = font + ":size=18";
        summary-font = font + ":size=15";
        body-font = font + ":size=14";
        border-size = 0;
      };
      low = {
        background = config.lib.stylix.colors.base00 + "e6";
        title-color = config.lib.stylix.colors.base03 + "ff";
        summary-color = config.lib.stylix.colors.base03 + "ff";
        body-color = config.lib.stylix.colors.base03 + "ff";
        idle-timeout = 150;
        max-timeout = 4;
        default-timeout = 2;
      };
      normal = {
        background = config.lib.stylix.colors.base00 + "e6";
        title-color = config.lib.stylix.colors.base07 + "ff";
        summary-color = config.lib.stylix.colors.base07 + "ff";
        body-color = config.lib.stylix.colors.base07 + "ff";
        idle-timeout = 150;
        max-timeout = 5;
        default-timeout = 3;
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
