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

  config = {
    userSettings.alacritty.enable = true;
    userSettings.kitty.enable = true;
    userSettings.japanese.enable = true;
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
      WLR_DRM_DEVICES = "/dev/dri/card2:/dev/dri/card1";
      GDK_BACKEND = "wayland,x11,*";
      QT_QPA_PLATFORM = "wayland;xcb";
      QT_QPA_PLATFORMTHEME = lib.mkForce "qt5ct";
      QT_AUTO_SCREEN_SCALE_FACTOR = 1;
      QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
      CLUTTER_BACKEND = "wayland";
      GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
      GSK_RENDERER = "gl";
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
      settings = { };
      systemd.variables = ["--all"];
      extraConfig = ''

        exec-once = hyprctl setcursor ${config.gtk.cursorTheme.name} ${builtins.toString config.gtk.cursorTheme.size}
        exec-once = sleep 10 && nextcloud
        exec-once = iio-hyprland
        exec-once = hyprprofile Default

        exec-once = ydotoold
        exec-once = sleep 10 && caffeine
        #exec-once = STEAM_FRAME_FORCE_CLOSE=1 steam -silent
        exec-once = nm-applet
        exec-once = blueman-applet
        exec-once = GOMAXPROCS=1 syncthing --no-browser
        exec-once = protonmail-bridge --noninteractive
        exec-once = eww open-many bar:first bar:second bar:third --arg first:monitor=0 --arg second:monitor=1 --arg third:monitor=2
        exec-once = hyprland-monitor-attached ~/.local/bin/eww-reload-bars.sh
        exec-once = emacs --daemon

        exec-once = hypridle
        exec-once = sleep 5 && libinput-gestures
        exec-once = obs-notification-mute-daemon

        exec-once = hyprpaper

        bezier = wind, 0.05, 0.9, 0.1, 1.05
        bezier = winIn, 0.1, 1.1, 0.1, 1.0
        bezier = winOut, 0.3, -0.3, 0, 1
        bezier = liner, 1, 1, 1, 1
        bezier = linear, 0.0, 0.0, 1.0, 1.0

        animations {
             enabled = yes
             animation = windowsIn, 1, 6, winIn, popin
             animation = windowsOut, 1, 5, winOut, popin
             animation = windowsMove, 1, 5, wind, slide
             animation = border, 1, 10, default
             animation = borderangle, 1, 100, linear, loop
             animation = fade, 1, 10, default
             animation = workspaces, 1, 5, wind
             animation = windows, 1, 6, wind, slide
             animation = specialWorkspace, 1, 6, default, slidefadevert -50%
        }

        general {
          layout = master
          border_size = 5
          col.active_border = 0xff'' + config.lib.stylix.colors.base08 + " " + ''0xff'' + config.lib.stylix.colors.base09 + " " + ''0xff'' + config.lib.stylix.colors.base0A + " " + ''0xff'' + config.lib.stylix.colors.base0B + " " + ''0xff'' + config.lib.stylix.colors.base0C + " " + ''0xff'' + config.lib.stylix.colors.base0D + " " + ''0xff'' + config.lib.stylix.colors.base0E + " " + ''0xff'' + config.lib.stylix.colors.base0F + " " + ''270deg

          col.inactive_border = 0xff'' + config.lib.stylix.colors.base02 + ''

              resize_on_border = true
              gaps_in = 7
              gaps_out = 7
         }

         cursor {
           no_warps = false
           inactive_timeout = 30
         }

         bind=SUPER,code:9,exec,nwggrid-wrapper
         bind=SUPER,code:66,exec,nwggrid-wrapper
         bind=SUPER,SPACE,fullscreen,1
         bind=SUPERSHIFT,F,fullscreen,0
         bind=SUPER,Y,workspaceopt,allfloat
         bind=ALT,TAB,cyclenext
         bind=ALT,TAB,bringactivetotop
         bind=ALTSHIFT,TAB,cyclenext,prev
         bind=ALTSHIFT,TAB,bringactivetotop
         bind=SUPER,V,exec,wl-copy $(wl-paste | tr '\n' ' ')
         bind=SUPERSHIFT,T,exec,screenshot-ocr
         bind=CTRLALT,Delete,exec,hyprctl kill
         bind=SUPERSHIFT,K,exec,hyprctl kill

         bind=,code:172,exec,mpc toggle
         bind=,code:208,exec,mpc toggle
         bind=,code:209,exec,mpc toggle
         bind=,code:174,exec,mpc stop
         bind=,code:171,exec,mpc next
         bind=,code:173,exec,mpc prev

         bind = SUPER,R,pass,^(com\.obsproject\.Studio)$
         bind = SUPERSHIFT,R,pass,^(com\.obsproject\.Studio)$

         bind=SUPER,RETURN,exec,'' + term + ''

         bind=SUPERSHIFT,RETURN,exec,'' + term + " " + '' --class float_term

         bind=SUPER,A,exec,'' + spawnEditor + ''

         bind=SUPER,S,exec,'' + spawnBrowser + ''

         bind=SUPERCTRL,S,exec,container-open # qutebrowser only

         bind=SUPERCTRL,P,pin

         bind=SUPER,code:47,exec,fuzzel
         bind=SUPER,X,exec,fnottctl dismiss
         bind=SUPERSHIFT,X,exec,fnottctl dismiss all
         bind=SUPER,Q,killactive
         bind=SUPERSHIFT,Q,exit
         bindm=SUPER,mouse:272,movewindow
         bindm=SUPER,mouse:273,resizewindow
         bind=SUPER,T,togglefloating
         bind=,code:148,exec,''+ term + " "+''-e numbat

         bind=,code:107,exec,grim -g "$(slurp)"
         bind=SHIFT,code:107,exec,grim -g "$(slurp -o)"
         bind=SUPER,code:107,exec,grim
         bind=CTRL,code:107,exec,grim -g "$(slurp)" - | wl-copy
         bind=SHIFTCTRL,code:107,exec,grim -g "$(slurp -o)" - | wl-copy
         bind=SUPERCTRL,code:107,exec,grim - | wl-copy

         bind=,code:122,exec,swayosd-client --output-volume lower
         bind=,code:123,exec,swayosd-client --output-volume raise
         bind=,code:121,exec,swayosd-client --output-volume mute-toggle
         bind=,code:256,exec,swayosd-client --output-volume mute-toggle
         bind=SHIFT,code:122,exec,swayosd-client --output-volume lower
         bind=SHIFT,code:123,exec,swayosd-client --output-volume raise
         bind=,code:232,exec,swayosd-client --brightness lower
         bind=,code:233,exec,swayosd-client --brightness raise
         bind=,code:237,exec,brightnessctl --device='asus::kbd_backlight' set 1-
         bind=,code:238,exec,brightnessctl --device='asus::kbd_backlight' set +1
         bind=,code:255,exec,airplane-mode
         bind=SUPER,C,exec,wl-copy $(hyprpicker)

         bind=SUPERSHIFT,S,exec,systemctl suspend
         bindl=,switch:on:Lid Switch,exec,loginctl lock-session
         bind=SUPERCTRL,L,exec,loginctl lock-session

         bind=SUPER,H,movefocus,l
         bind=SUPER,J,movefocus,d
         bind=SUPER,K,movefocus,u
         bind=SUPER,L,movefocus,r

         bind=SUPERSHIFT,H,movewindow,l
         bind=SUPERSHIFT,J,movewindow,d
         bind=SUPERSHIFT,K,movewindow,u
         bind=SUPERSHIFT,L,movewindow,r

         bind=SUPER,1,focusworkspaceoncurrentmonitor,1
         bind=SUPER,2,focusworkspaceoncurrentmonitor,2
         bind=SUPER,3,focusworkspaceoncurrentmonitor,3
         bind=SUPER,4,focusworkspaceoncurrentmonitor,4
         bind=SUPER,5,focusworkspaceoncurrentmonitor,5
         bind=SUPER,6,focusworkspaceoncurrentmonitor,6
         bind=SUPER,7,focusworkspaceoncurrentmonitor,7
         bind=SUPER,8,focusworkspaceoncurrentmonitor,8
         bind=SUPER,9,focusworkspaceoncurrentmonitor,9

         bind=SUPERCTRL,right,exec,hyprnome
         bind=SUPERCTRL,left,exec,hyprnome --previous
         bind=SUPERSHIFT,right,exec,hyprnome --move
         bind=SUPERSHIFT,left,exec,hyprnome --previous --move

         bind=SUPERSHIFT,1,movetoworkspace,1
         bind=SUPERSHIFT,2,movetoworkspace,2
         bind=SUPERSHIFT,3,movetoworkspace,3
         bind=SUPERSHIFT,4,movetoworkspace,4
         bind=SUPERSHIFT,5,movetoworkspace,5
         bind=SUPERSHIFT,6,movetoworkspace,6
         bind=SUPERSHIFT,7,movetoworkspace,7
         bind=SUPERSHIFT,8,movetoworkspace,8
         bind=SUPERSHIFT,9,movetoworkspace,9

         exec-once = alacritty --class scratch_term
         exec-once = kitty --class scratch_ranger -e ranger
         exec-once = alacritty --class scratch_numbat -e numbat
         exec-once = kitty --class scratch_music -e ncmpcpp
         exec-once = alacritty --class scratch_btm -e btm
         exec-once = element-desktop
         exec-once = pavucontrol

         bind=SUPER,Z,exec,if hyprctl clients | grep scratch_term; then echo "scratch_term respawn not needed"; else alacritty --class scratch_term; fi
         bind=SUPER,Z,togglespecialworkspace,scratch_term
         bind=SUPER,F,exec,if hyprctl clients | grep scratch_ranger; then echo "scratch_ranger respawn not needed"; else kitty --class scratch_ranger -e ranger; fi
         bind=SUPER,F,togglespecialworkspace,scratch_ranger
         bind=SUPER,N,exec,if hyprctl clients | grep scratch_numbat; then echo "scratch_ranger respawn not needed"; else alacritty --class scratch_numbat -e numbat; fi
         bind=SUPER,N,togglespecialworkspace,scratch_numbat
         bind=SUPER,M,exec,if hyprctl clients | grep scratch_music; then echo "scratch_music respawn not needed"; else kitty --class scratch_music -e ncmpcpp; fi
         bind=SUPER,M,togglespecialworkspace,scratch_music
         bind=SUPER,B,exec,if hyprctl clients | grep scratch_btm; then echo "scratch_ranger respawn not needed"; else alacritty --class scratch_btm -e btm; fi
         bind=SUPER,B,togglespecialworkspace,scratch_btm
         bind=SUPER,D,exec,if hyprctl clients | grep Element; then echo "scratch_ranger respawn not needed"; else element-desktop; fi
         bind=SUPER,D,togglespecialworkspace,scratch_element
         bind=SUPER,code:172,exec,togglespecialworkspace,scratch_pavucontrol
         bind=SUPER,code:172,exec,if hyprctl clients | grep pavucontrol; then echo "scratch_ranger respawn not needed"; else pavucontrol; fi

         $scratchpadsize = size 80% 85%

         $scratch_term = class:^(scratch_term)$
         windowrulev2 = float,$scratch_term
         windowrulev2 = $scratchpadsize,$scratch_term
         windowrulev2 = workspace special:scratch_term silent ,$scratch_term
         windowrulev2 = center,$scratch_term

         $float_term = class:^(float_term)$
         windowrulev2 = float,$float_term
         windowrulev2 = center,$float_term

         $scratch_ranger = class:^(scratch_ranger)$
         windowrulev2 = float,$scratch_ranger
         windowrulev2 = $scratchpadsize,$scratch_ranger
         windowrulev2 = workspace special:scratch_ranger silent,$scratch_ranger
         windowrulev2 = center,$scratch_ranger

         $scratch_numbat = class:^(scratch_numbat)$
         windowrulev2 = float,$scratch_numbat
         windowrulev2 = $scratchpadsize,$scratch_numbat
         windowrulev2 = workspace special:scratch_numbat silent,$scratch_numbat
         windowrulev2 = center,$scratch_numbat

         $scratch_btm = class:^(scratch_btm)$
         windowrulev2 = float,$scratch_btm
         windowrulev2 = $scratchpadsize,$scratch_btm
         windowrulev2 = workspace special:scratch_btm silent,$scratch_btm
         windowrulev2 = center,$scratch_btm

         windowrulev2 = float,class:^(Element)$
         windowrulev2 = size 85% 90%,class:^(Element)$
         windowrulev2 = workspace special:scratch_element silent,class:^(Element)$
         windowrulev2 = center,class:^(Element)$

         $scratch_music = class:^(scratch_music)$
         windowrulev2 = float,$scratch_music
         windowrulev2 = $scratchpadsize,$scratch_music
         windowrulev2 = workspace special:scratch_music silent,$scratch_music
         windowrulev2 = center,$scratch_music

         $savetodisk = title:^(Save to Disk)$
         windowrulev2 = float,$savetodisk
         windowrulev2 = size 70% 75%,$savetodisk
         windowrulev2 = center,$savetodisk

         $pavucontrol = class:^(org.pulseaudio.pavucontrol)$
         windowrulev2 = float,$pavucontrol
         windowrulev2 = size 86% 40%,$pavucontrol
         windowrulev2 = move 50% 6%,$pavucontrol
         windowrulev2 = workspace special silent,$pavucontrol
         windowrulev2 = opacity 0.80,$pavucontrol

         $miniframe = title:\*Minibuf.*
         windowrulev2 = float,$miniframe
         windowrulev2 = size 64% 50%,$miniframe
         windowrulev2 = move 18% 25%,$miniframe
         windowrulev2 = animation popin 1 20,$miniframe

         windowrulev2 = float,class:^(pokefinder)$
         windowrulev2 = float,class:^(Waydroid)$

         windowrulev2 = float,title:(Blender Render)
         windowrulev2 = size 86% 85%,title:(Blender Render)
         windowrulev2 = center,title:(Blender Render)
         windowrulev2 = float,class:^(org.inkscape.Inkscape)$
         windowrulev2 = float,class:^(pinta)$
         windowrulev2 = float,class:^(krita)$
         windowrulev2 = float,class:^(Gimp)
         windowrulev2 = float,class:^(Gimp)
         windowrulev2 = float,class:^(libresprite)$
         windowrulev2 = float,title:(Open Images)
         windowrulev2 = size 86% 85%,title:(Open Images)
         windowrulev2 = center,title:(Open Images)
         windowrulev2 = float,title:(Create new document)
         windowrulev2 = size 86% 85%,title:(Create new document)
         windowrulev2 = center,title:(Create new document)
         windowrulev2 = size 86% 85%,title:(Create new document)
         windowrulev2 = float,title:(Create New Node)
         windowrulev2 = size 70% 70%,title:(Create New Node)
         windowrulev2 = center,title:(Create New Node)
         windowrulev2 = float,title:(Resource)
         windowrulev2 = size 70% 70%,title:(Resource)
         windowrulev2 = center,title:(Resource)
         windowrulev2 = tile,title:(Godot)

         windowrulev2 = opacity 0.80,title:ORUI

         windowrulev2 = opacity 1.0,class:^(org.qutebrowser.qutebrowser),fullscreen:1
         windowrulev2 = opacity 0.85,class:^(Element)$
         windowrulev2 = opacity 0.85,class:^(Logseq)$
         windowrulev2 = opacity 1.0,class:^(Brave-browser),fullscreen:1
         windowrulev2 = opacity 1.0,class:^(librewolf),fullscreen:1
         windowrulev2 = opacity 0.85,title:^(My Local Dashboard Awesome Homepage - qutebrowser)$
         windowrulev2 = opacity 0.85,title:\[.*\] - My Local Dashboard Awesome Homepage
         windowrulev2 = opacity 0.85,class:^(org.keepassxc.KeePassXC)$
         windowrulev2 = opacity 0.85,class:^(org.gnome.Nautilus)$
         windowrulev2 = opacity 0.85,class:^(org.gnome.Nautilus)$

         windowrulev2 = opacity 0.85,initialTitle:^(Notes)$,initialClass:^(Brave-browser)$

         layerrule = blur,waybar
         layerrule = xray 1,waybar
         blurls = waybar
         layerrule = ignorezero, eww
         layerrule = xray 1,eww
         layerrule = blur,launcher # fuzzel
         blurls = launcher # fuzzel
         layerrule = blur,gtk-layer-shell
         layerrule = xray 1,gtk-layer-shell
         layerrule = ignorezero, gtk-layer-shell
         layerrule = blur,eww
         layerrule = xray 1,eww
         layerrule = ignorezero, eww
         layerrule = animation popin 80%, eww
         blurls = gtk-layer-shell
         layerrule = blur,~nwggrid
         layerrule = xray 1,~nwggrid
         layerrule = animation fade,~nwggrid
         blurls = ~nwggrid

         bind=SUPER,equal, exec, hyprctl keyword cursor:zoom_factor "$(hyprctl getoption cursor:zoom_factor | grep float | awk '{print $2 + 0.5}')"
         bind=SUPER,minus, exec, hyprctl keyword cursor:zoom_factor "$(hyprctl getoption cursor:zoom_factor | grep float | awk '{print $2 - 0.5}')"

         bind=SUPER,I,exec,networkmanager_dmenu
         bind=SUPER,P,exec,keepmenu
         bind=SUPERSHIFT,P,exec,hyprprofile-dmenu
         bind=SUPERCTRL,R,exec,phoenix refresh

         # 3 monitor setup
         monitor=eDP-1,1920x1080@300,900x1080,1
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

         binds {
           movefocus_cycles_fullscreen = false
         }

         input {
           kb_layout = us
           kb_options = caps:escape
           repeat_delay = 450
           repeat_rate = 50
           accel_profile = adaptive
           follow_mouse = 2
           float_switch_override_focus = 0
         }

         misc {
           disable_hyprland_logo = true
           mouse_move_enables_dpms = true
           enable_swallow = true
           swallow_regex = (scratch_term)|(Alacritty)|(kitty)
           font_family = '' + font + ''

         }
         decoration {
           rounding = 8
           dim_special = 0.0
           blur {
             enabled = true
             size = 5
             passes = 2
             ignore_opacity = true
             contrast = 1.17
             brightness = '' + (if (config.stylix.polarity == "dark") then "0.65" else "1.45") + ''

             xray = true
             special = true
             popups = true
           }
         }

      '';
      xwayland = { enable = true; };
      systemd.enable = true;
    };

    home.packages = (with pkgs; [
      hyprland-monitor-attached
      caffeine-ng
      alacritty
      kitty
      killall
      polkit_gnome
      eww
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
      pamixer
      tesseract4
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
      workspace=$(hyprctl monitors -j | jq ".[] | select(.specialWorkspace.name == \"special:scratch_ranger\") | .activeWorkspace.id")
      if [ -z "''${workspace}" ]; then
        hyprctl dispatch exec -- "$command";
      else
        hyprctl dispatch exec "[workspace $workspace]" -- "$command" "$file";
      fi
      hyprctl dispatch togglespecialworkspace scratch_ranger
      '')
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
    home.file.".local/bin/eww-reload-bars.sh" = {
      text = ''#!/bin/sh
               eww open-many bar:first bar:second bar:third --arg first:monitor=0 --arg second:monitor=1 --arg third:monitor=2;'';
      executable = true;
    };
    home.file.".config/hypr/hypridle.conf".text = ''
      general {
        lock_cmd = pgrep hyprlock || hyprlock
        before_sleep_cmd = loginctl lock-session
        ignore_dbus_inhibit = false
      }

      #listener {
      #  timeout = 150 # in seconds
      #  on-timeout = hyprctl dispatch dpms off
      #  on-resume = hyprctl dispatch dpms on
      #}
      listener {
        timeout = 165 # in seconds
        on-timeout = loginctl lock-session
      }
      listener {
        timeout = 180 # in seconds
        #timeout = 5400 # in seconds
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
    services.cbatticon = {
      enable = true;
      iconType = "symbolic";
    };
    home.file.".config/eww/eww.yuck".source = ./eww/eww.yuck;
    home.file = {
      ".config/eww/eww.scss".source = config.lib.stylix.colors {
        template = builtins.readFile ./eww/eww.scss.mustache;
        extension = ".scss";
      };
    };
    home.file.".config/nwg-launchers/nwggrid/style.css".text = ''
      button, label, image {
          background: none;
          border-style: none;
          box-shadow: none;
          color: #'' + config.lib.stylix.colors.base07 + '';

          font-size: 20px;
      }

      button {
          padding: 5px;
          margin: 5px;
          text-shadow: none;
      }

      button:hover {
          background-color: rgba('' + config.lib.stylix.colors.base07-rgb-r + "," + config.lib.stylix.colors.base07-rgb-g + "," + config.lib.stylix.colors.base07-rgb-b + "," + ''0.15);
      }

      button:focus {
          box-shadow: 0 0 10px;
      }

      button:checked {
          background-color: rgba('' + config.lib.stylix.colors.base07-rgb-r + "," + config.lib.stylix.colors.base07-rgb-g + "," + config.lib.stylix.colors.base07-rgb-b + "," + ''0.15);
      }

      #searchbox {
          background: none;
          border-color: #'' + config.lib.stylix.colors.base07 + '';

          color: #'' + config.lib.stylix.colors.base07 + '';

          margin-top: 20px;
          margin-bottom: 20px;

          font-size: 20px;
      }

      #separator {
          background-color: rgba('' + config.lib.stylix.colors.base00-rgb-r + "," + config.lib.stylix.colors.base00-rgb-g + "," + config.lib.stylix.colors.base00-rgb-b + "," + ''0.55);

          color: #'' + config.lib.stylix.colors.base07 + '';
          margin-left: 500px;
          margin-right: 500px;
          margin-top: 10px;
          margin-bottom: 10px
      }

      #description {
          margin-bottom: 20px
      }
    '';
    home.file.".config/nwg-launchers/nwggrid/terminal".text = "alacritty -e";

    services.udiskie.enable = true;
    services.udiskie.tray = "always";
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
      preload = ''+config.stylix.image+''

      wallpaper = ,''+config.stylix.image+''

    '';

  };
}
