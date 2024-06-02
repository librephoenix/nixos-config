{ inputs, config, lib, pkgs, userSettings, systemSettings, ... }: let
  pkgs-hyprland = inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in
{
  imports = [
    ../../app/terminal/alacritty.nix
    ../../app/terminal/kitty.nix
    (import ../../app/dmenu-scripts/networkmanager-dmenu.nix {
      #dmenu_command = "fuzzel -d -f ${userSettings.font} :size=16"; inherit config lib pkgs;
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
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    plugins = [
      inputs.hyprland-plugins.packages.${pkgs.system}.hyprtrails
      inputs.hycov.packages.${pkgs.system}.hycov
      inputs.hyprgrass.packages.${pkgs.system}.default
    ];
    settings = { };
    extraConfig = ''
      exec-once = dbus-update-activation-environment DISPLAY XAUTHORITY WAYLAND_DISPLAY
      exec-once = hyprctl setcursor '' + config.gtk.cursorTheme.name + " " + builtins.toString config.gtk.cursorTheme.size + ''

      env = XDG_CURRENT_DESKTOP,Hyprland
      env = XDG_SESSION_TYPE,wayland
      env = XDG_SESSION_DESKTOP,Hyprland
      env = WLR_DRM_DEVICES,/dev/dri/card2:/dev/dri/card1
      env = GDK_BACKEND,wayland,x11,*
      env = QT_QPA_PLATFORM,wayland;xcb
      env = QT_QPA_PLATFORMTHEME,qt5ct
      env = QT_AUTO_SCREEN_SCALE_FACTOR,1
      env = QT_WAYLAND_DISABLE_WINDOWDECORATION,1
      env = CLUTTER_BACKEND,wayland

      exec-once = hyprprofile Default

      exec-once = pypr
      exec-once = ydotoold
      #exec-once = STEAM_FRAME_FORCE_CLOSE=1 steam -silent
      exec-once = nm-applet
      exec-once = blueman-applet
      exec-once = hypr-element-start
      exec-once = GOMAXPROCS=1 syncthing --no-browser
      exec-once = protonmail-bridge --noninteractive
      exec-once = waybar
      exec-once = emacs --daemon

      exec-once = hypridle
      exec-once = obs-notification-mute-daemon

      exec-once = hyprpaper

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

       plugin {
         hyprtrails {
             color = rgba(''+config.lib.stylix.colors.base08+''55)
         }
         hycov {
             overview_gappo = 60 # gaps width from screen edge
             overview_gappi = 24 # gaps width from clients
             enable_hotarea = 0 # enable mouse cursor hotarea, when cursor enter hotarea, it will toggle overview
             enable_click_action = 1 # enable mouse left button jump and right button kill in overview mode
             hotarea_monitor = all # monitor name which hotarea is in, default is all
             hotarea_pos = 1 # position of hotarea (1: bottom left, 2: bottom right, 3: top left, 4: top right)
             hotarea_size = 10 # hotarea size, 10x10
             swipe_fingers = 3 # finger number of gesture,move any directory
             move_focus_distance = 100 # distance for movefocus,only can use 3 finger to move
             enable_gesture = 1 # enable gesture
             auto_exit = 1 # enable auto exit when no client in overview
             auto_fullscreen = 0 # auto make active window maximize after exit overview
             only_active_workspace = 0 # only overview the active workspace
             only_active_monitor = 0 # only overview the active monitor
             enable_alt_release_exit = 0 # alt swith mode arg,see readme for detail
             alt_replace_key = Super_L # alt swith mode arg,see readme for detail
             alt_toggle_auto_next = 0 # auto focus next window when toggle overview in alt swith mode
             click_in_cursor = 1 # when click to jump,the target windwo is find by cursor, not the current foucus window.
             hight_of_titlebar = 0 # height deviation of title bar height
             show_special = 0 # show windwos in special workspace in overview.

         }
         touch_gestures {
             sensitivity = 4.0
             long_press_delay = 260
             hyprgrass-bind = , edge:r:l, exec, hyprnome
             hyprgrass-bind = , edge:l:r, exec, hyprnome --previous
             hyprgrass-bind = , swipe:3:d, exec, nwg-drawer
             hyprgrass-bind = , swipe:3:u, hycov:toggleoverview
             hyprgrass-bind = , swipe:3:r, exec, hyprnome
             hyprgrass-bind = , swipe:3:l, exec, hyprnome --previous
             hyprgrass-bindm = , longpress:2, movewindow
             hyprgrass-bindm = , longpress:3, resizewindow
         }
       }

       bind=SUPER,SPACE,fullscreen,1
       bind=SUPERSHIFT,F,fullscreen,0
       bind=SUPER,Y,workspaceopt,allfloat
       bind=ALT,TAB,cyclenext
       bind=ALT,TAB,bringactivetotop
       bind=ALTSHIFT,TAB,cyclenext,prev
       bind=ALTSHIFT,TAB,bringactivetotop
       bind=SUPER,TAB,hycov:toggleoverview
       bind=SUPER,left,hycov:movefocus,leftcross
       bind=SUPER,right,hycov:movefocus,rightcross
       bind=SUPER,up,hycov:movefocus,upcross
       bind=SUPER,down,hycov:movefocus,downcross
       bind=SUPER,V,exec,wl-copy $(wl-paste | tr '\n' ' ')
       bind=SUPERSHIFT,T,exec,screenshot-ocr
       bind=CTRLALT,Delete,exec,hyprctl kill
       bind=SUPERSHIFT,K,exec,hyprctl kill
       bind=SUPER,W,exec,nwg-dock-wrapper

       bind = SUPER,R,pass,^(com\.obsproject\.Studio)$
       bind = SUPERSHIFT,R,pass,^(com\.obsproject\.Studio)$

       bind=SUPER,RETURN,exec,'' + userSettings.term + ''

       bind=SUPER,A,exec,'' + userSettings.spawnEditor + ''

       bind=SUPER,S,exec,'' + userSettings.browser + ''

       bind=SUPERCTRL,S,exec,container-open # qutebrowser only

       bind=SUPERCTRL,R,exec,phoenix refresh

       bind=SUPER,code:47,exec,fuzzel
       bind=SUPER,X,exec,fnottctl dismiss
       bind=SUPERSHIFT,X,exec,fnottctl dismiss all
       bind=SUPER,Q,killactive
       bind=SUPERSHIFT,Q,exit
       bindm=SUPER,mouse:272,movewindow
       bindm=SUPER,mouse:273,resizewindow
       bind=SUPER,T,togglefloating
       bind=SUPER,G,exec,hyprctl dispatch focusworkspaceoncurrentmonitor 9; pegasus-fe;
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

       bind=SUPER,Z,exec,pypr toggle term && hyprctl dispatch bringactivetotop
       bind=SUPER,F,exec,pypr toggle ranger && hyprctl dispatch bringactivetotop
       bind=SUPER,N,exec,pypr toggle numbat && hyprctl dispatch bringactivetotop
       bind=SUPER,M,exec,pypr toggle musikcube && hyprctl dispatch bringactivetotop
       bind=SUPER,B,exec,pypr toggle btm && hyprctl dispatch bringactivetotop
       bind=SUPER,D,exec,hypr-element
       bind=SUPER,code:172,exec,pypr toggle pavucontrol && hyprctl dispatch bringactivetotop
       $scratchpadsize = size 80% 85%

       $scratchpad = class:^(scratchpad)$
       windowrulev2 = float,$scratchpad
       windowrulev2 = $scratchpadsize,$scratchpad
       windowrulev2 = workspace special silent,$scratchpad
       windowrulev2 = center,$scratchpad

       windowrulev2 = float,class:^(Element)$
       windowrulev2 = size 85% 90%,class:^(Element)$
       windowrulev2 = center,class:^(Element)$

       $savetodisk = title:^(Save to Disk)$
       windowrulev2 = float,$savetodisk
       windowrulev2 = size 70% 75%,$savetodisk
       windowrulev2 = center,$savetodisk

       $pavucontrol = class:^(pavucontrol)$
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

       windowrulev2 = opacity 0.80,title:ORUI

       windowrulev2 = opacity 1.0,class:^(org.qutebrowser.qutebrowser),fullscreen:1
       windowrulev2 = opacity 0.90,class:^(Element)$
       windowrulev2 = opacity 1.0,class:^(Brave-browser),fullscreen:1
       windowrulev2 = opacity 1.0,class:^(librewolf),fullscreen:1
       windowrulev2 = opacity 0.80,title:^(LibreWolf)$
       windowrulev2 = opacity 0.80,title:^(New Tab - LibreWolf)$
       windowrulev2 = opacity 0.80,title:^(New Tab - Brave)$
       windowrulev2 = opacity 0.75,title:^(My Local Dashboard Awesome Homepage - qutebrowser)$
       windowrulev2 = opacity 0.75,title:\[.*\] - My Local Dashboard Awesome Homepage
       windowrulev2 = opacity 0.9,class:^(org.keepassxc.KeePassXC)$
       windowrulev2 = opacity 0.75,class:^(org.gnome.Nautilus)$
       windowrulev2 = opacity 0.75,class:^(org.gnome.Nautilus)$

       layerrule = blur,waybar
       layerrule = xray,waybar
       blurls = waybar
       layerrule = blur,launcher # fuzzel
       blurls = launcher # fuzzel
       layerrule = blur,gtk-layer-shell
       layerrule = xray,gtk-layer-shell
       blurls = gtk-layer-shell

       bind=SUPER,code:21,exec,pypr zoom
       bind=SUPER,code:21,exec,hyprctl reload

       bind=SUPER,I,exec,networkmanager_dmenu
       bind=SUPER,P,exec,keepmenu
       bind=SUPERSHIFT,P,exec,hyprprofile-dmenu

       # 3 monitor setup
       monitor=eDP-1,1920x1080,900x1080,1
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
           xray = true
         }
       }

    '';
    xwayland = { enable = true; };
    systemd.enable = true;
  };

  home.packages = (with pkgs; [
    alacritty
    kitty
    feh
    killall
    polkit_gnome
    (nwg-dock-hyprland.overrideAttrs (oldAttrs: {
      patches = ./patches/noactiveclients.patch;
    }))
    libva-utils
    gsettings-desktop-schemas
    (pyprland.overrideAttrs (oldAttrs: {
      src = fetchFromGitHub {
        owner = "hyprland-community";
        repo = "pyprland";
        rev = "refs/tags/2.2.17";
        hash = "sha256-S1bIIazrBWyjF8tOcIk0AwwWq9gbpTKNsjr9iYA5lKk=";
      };
    }))
    (hyprnome.override (oldAttrs: {
        rustPlatform = oldAttrs.rustPlatform // {
          buildRustPackage = args: oldAttrs.rustPlatform.buildRustPackage (args // {
            pname = "hyprnome";
            version = "unstable-2024-05-06";
            src = fetchFromGitHub {
              owner = "donovanglover";
              repo = "hyprnome";
              rev = "f185e6dbd7cfcb3ecc11471fab7d2be374bd5b28";
              hash = "sha256-tmko/bnGdYOMTIGljJ6T8d76NPLkHAfae6P6G2Aa2Qo=";
            };
            cargoDeps = oldAttrs.cargoDeps.overrideAttrs (oldAttrs: rec {
              name = "${pname}-vendor.tar.gz";
              inherit src;
              outputHash = "sha256-cQwAGNKTfJTnXDI3IMJQ2583NEIZE7GScW7TsgnKrKs=";
            });
            cargoHash = "sha256-cQwAGNKTfJTnXDI3IMJQ2583NEIZE7GScW7TsgnKrKs=";
          });
        };
     })
    )
    gnome.zenity
    wlr-randr
    wtype
    ydotool
    wl-clipboard
    hyprland-protocols
    hyprpicker
    hypridle
    hyprpaper
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
    (pkgs.writeScriptBin "nwg-dock-wrapper" ''
      #!/bin/sh
      if pgrep -x ".nwg-dock-hyprl" > /dev/null
      then
        nwg-dock-hyprland
      else
        nwg-dock-hyprland -f -x -i 64 -nolauncher -a start -ml 8 -mr 8 -mb 8
      fi
    '')
    (pkgs.writeScriptBin "hypr-element-start" ''
      #!/usr/bin/env sh
      sleep 6 && element-desktop --hidden
    '')
    (pkgs.writeScriptBin "hypr-element" ''
      #!/bin/sh
      if hyprctl clients | grep "class: Element" > /dev/null
      then
        hyprctl dispatch closewindow Element
      else
        element-desktop
      fi
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
    (pkgs.makeDesktopItem {
      name = "emacsclientnewframe";
      desktopName = "Emacs Client New Frame";
      exec = "emacsclient -c -a emacs";
      terminal = false;
      icon = "emacs";
      type = "Application";
    })])
  ++
  (with pkgs-hyprland; [ hyprlock ]);
  home.file.".config/nwg-dock-hyprland/style.css".text = ''
    window {
      background: rgba(''+config.lib.stylix.colors.base00-rgb-r+'',''+config.lib.stylix.colors.base00-rgb-g+'',''+config.lib.stylix.colors.base00-rgb-b+'',0.0);
      border-radius: 20px;
      padding: 4px;
      margin-left: 4px;
      margin-right: 4px;
      border-style: none;
    }

    #box {
      /* Define attributes of the box surrounding icons here */
      padding: 10px;
      background: rgba(''+config.lib.stylix.colors.base00-rgb-r+'',''+config.lib.stylix.colors.base00-rgb-g+'',''+config.lib.stylix.colors.base00-rgb-b+'',0.55);
      border-radius: 20px;
      padding: 4px;
      margin-left: 4px;
      margin-right: 4px;
      border-style: none;
    }
    button {
      border-radius: 10px;
      padding: 4px;
      margin-left: 4px;
      margin-right: 4px;
      background: rgba(''+config.lib.stylix.colors.base03-rgb-r+'',''+config.lib.stylix.colors.base03-rgb-g+'',''+config.lib.stylix.colors.base03-rgb-b+'',0.55);
      color: #''+config.lib.stylix.colors.base07+'';
      font-size: 12px
    }

    button:hover {
      background: rgba(''+config.lib.stylix.colors.base04-rgb-r+'',''+config.lib.stylix.colors.base04-rgb-g+'',''+config.lib.stylix.colors.base04-rgb-b+'',0.55);
    }

  '';
  home.file.".config/nwg-dock-pinned".text = ''
    Alacritty
    emacsclientnewframe
    qutebrowser
    brave-browser
    librewolf
    writer
    impress
    calc
    draw
    krita
    pinta
    xournalpp
    obs
    kdenlive
    blender
    openscad
    Cura
    virt-manager
  '';
  home.file.".config/hypr/hypridle.conf".text = ''
    general {
      lock_cmd = pgrep hyprlock || hyprlock
      before_sleep_cmd = loginctl lock-session
      ignore_dbus_inhibit = false
    }

    listener {
      timeout = 300 # in seconds
      on-timeout = loginctl lock-session
    }
    listener {
      timeout = 600 # in seconds
      on-timeout = systemctl suspend
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

    # doesn't work yet
    image {
      monitor =
      path = /home/emmet/.dotfiles/user/wm/hyprland/nix-dark.png
      size = 150 # lesser side if not 1:1 ratio
      rounding = -1 # negative values mean circle
      border_size = 0
      rotate = 0 # degrees, counter-clockwise

      position = 0, 200
      halign = center
      valign = center
    }

    input-field {
      monitor =
      size = 200, 50
      outline_thickness = 3
      dots_size = 0.33 # Scale of input-field height, 0.2 - 0.8
      dots_spacing = 0.15 # Scale of dots' absolute size, 0.0 - 1.0
      dots_center = false
      dots_rounding = -1 # -1 default circle, -2 follow input-field rounding
      outer_color = rgb(''+config.lib.stylix.colors.base07-rgb-r+'',''+config.lib.stylix.colors.base07-rgb-g+'', ''+config.lib.stylix.colors.base07-rgb-b+'')
      inner_color = rgb(''+config.lib.stylix.colors.base00-rgb-r+'',''+config.lib.stylix.colors.base00-rgb-g+'', ''+config.lib.stylix.colors.base00-rgb-b+'')
      font_color = rgb(''+config.lib.stylix.colors.base07-rgb-r+'',''+config.lib.stylix.colors.base07-rgb-g+'', ''+config.lib.stylix.colors.base07-rgb-b+'')
      fade_on_empty = true
      fade_timeout = 1000 # Milliseconds before fade_on_empty is triggered.
      placeholder_text = <i>Input Password...</i> # Text rendered in the input box when it's empty.
      hide_input = false
      rounding = -1 # -1 means complete rounding (circle/oval)
      check_color = rgb(''+config.lib.stylix.colors.base0A-rgb-r+'',''+config.lib.stylix.colors.base0A-rgb-g+'', ''+config.lib.stylix.colors.base0A-rgb-b+'')
      fail_color = rgb(''+config.lib.stylix.colors.base08-rgb-r+'',''+config.lib.stylix.colors.base08-rgb-g+'', ''+config.lib.stylix.colors.base08-rgb-b+'')
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
      text = Hello, Emmet
      color = rgb(''+config.lib.stylix.colors.base07-rgb-r+'',''+config.lib.stylix.colors.base07-rgb-g+'', ''+config.lib.stylix.colors.base07-rgb-b+'')
      font_size = 25
      font_family = ''+userSettings.font+''
      rotate = 0 # degrees, counter-clockwise

      position = 0, 160
      halign = center
      valign = center
    }

    label {
      monitor =
      text = $TIME
      color = rgb(''+config.lib.stylix.colors.base07-rgb-r+'',''+config.lib.stylix.colors.base07-rgb-g+'', ''+config.lib.stylix.colors.base07-rgb-b+'')
      font_size = 20
      font_family = Intel One Mono
      rotate = 0 # degrees, counter-clockwise

      position = 0, 80
      halign = center
      valign = center
    }
  '';
  home.file.".config/hypr/pyprland.toml".text = ''
    [pyprland]
    plugins = ["scratchpads", "magnify"]

    [scratchpads.term]
    command = "alacritty --class scratchpad"
    margin = 50

    [scratchpads.ranger]
    command = "kitty --class scratchpad -e ranger"
    margin = 50

    [scratchpads.numbat]
    command = "alacritty --class scratchpad -e numbat"
    margin = 50

    [scratchpads.musikcube]
    command = "alacritty --class scratchpad -e musikcube"
    margin = 50

    [scratchpads.btm]
    command = "alacritty --class scratchpad -e btm"
    margin = 50

    [scratchpads.pavucontrol]
    command = "pavucontrol"
    margin = 50
    unfocus = "hide"
    animation = "fromTop"
  '';

  programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oldAttrs: {
      postPatch = ''
        # use hyprctl to switch workspaces
        sed -i 's/zext_workspace_handle_v1_activate(workspace_handle_);/const std::string command = "hyprctl dispatch focusworkspaceoncurrentmonitor " + std::to_string(id());\n\tsystem(command.c_str());/g' src/modules/wlr/workspace_manager.cpp
        sed -i 's/gIPC->getSocket1Reply("dispatch workspace " + std::to_string(id()));/gIPC->getSocket1Reply("dispatch focusworkspaceoncurrentmonitor " + std::to_string(id()));/g' src/modules/hyprland/workspaces.cpp
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
          background-color: rgba('' + config.lib.stylix.colors.base00-rgb-r + "," + config.lib.stylix.colors.base00-rgb-g + "," + config.lib.stylix.colors.base00-rgb-b + "," + ''0.55);
          border-radius: 8px;
          color: #'' + config.lib.stylix.colors.base07 + '';
          transition-property: background-color;
          transition-duration: .2s;
      }

      tooltip {
        color: #'' + config.lib.stylix.colors.base07 + '';
        background-color: rgba('' + config.lib.stylix.colors.base00-rgb-r + "," + config.lib.stylix.colors.base00-rgb-g + "," + config.lib.stylix.colors.base00-rgb-b + "," + ''0.9);
        border-style: solid;
        border-width: 3px;
        border-radius: 8px;
        border-color: #'' + config.lib.stylix.colors.base08 + '';
      }

      tooltip * {
        color: #'' + config.lib.stylix.colors.base07 + '';
        background-color: rgba('' + config.lib.stylix.colors.base00-rgb-r + "," + config.lib.stylix.colors.base00-rgb-g + "," + config.lib.stylix.colors.base00-rgb-b + "," + ''0.0);
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
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      font = userSettings.font + ":size=20";
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
