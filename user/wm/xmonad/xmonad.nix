{ config, pkgs, ... }:

{

  imports = [ ../picom/picom.nix
              ../../lang/haskell/haskell.nix
              ../../app/terminal/alacritty.nix
              ../../app/terminal/kitty.nix
              ( import ../../app/dmenu-scripts/networkmanager-dmenu.nix {dmenu_command = "rofi -show dmenu"; inherit pkgs;})
            ];

  home.packages = with pkgs; [
    xmobar
    networkmanagerapplet
    dunst
    pamixer
    autorandr
    alacritty
    kitty
    dmenu
    rofi
    keepmenu
    networkmanager_dmenu
    pavucontrol
    feh
    flameshot
    alttab
    xdotool
    xclip
    ddcutil
    sct
    libnotify
    xorg.xkill
    killall
    bottom
    brightnessctl
    xorg.xcursorthemes
    xorg.xev
    xdg-utils
    xdg-desktop-portal
    xdg-desktop-portal-gtk
  ];

  home.file.".config/xmonad/xmonad.hs".source = ./xmonad.hs;
  home.file.".config/xmonad/startup.sh".source = ./startup.sh;

  home.file.".config/xmonad/lib/Colors/Stylix.hs".source = config.lib.stylix.colors {
    template = builtins.readFile ./lib/Colors/Stylix.hs.mustache;
    extension = ".hs";
  };

  home.file.".config/xmobar/xmobarrc".source = config.lib.stylix.colors {
    template = builtins.readFile ./xmobarrc.mustache;
    extension = "";
  };


  home.file.".config/xmobar/xmobar-st-check.sh" = {
    source = config.lib.stylix.colors {
      template = builtins.readFile ./xmobar-st-check.sh.mustache;
      extension = ".sh";
    };
    executable = true;
  };

  programs.feh.enable = true;
  programs.rofi.enable = true;

  services.autorandr.enable = true;
  programs.autorandr.enable = true;
  programs.autorandr.profiles = {
      "default" = {
        fingerprint = {
          eDP1 = "00ffffffffffff0051b8601500000000171e0104a522137807ee91a3544c99260f5054000000010101010101010101010101010101011434805070381f402b20750458c210000018000000fd0e302d505043010a20202020202000000010000a202020202020202020202020000000fc00544c3135365644585030310a2001d67013790000030114630401847f074f002a001f0037041e00160004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000df90";
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
          };
        };
        hooks.postswitch = "xmonad --restart; ~/.fehbg-stylix;";
      };
      "dock" = {
        fingerprint = {
          eDP1 = "00ffffffffffff0051b8601500000000171e0104a522137807ee91a3544c99260f5054000000010101010101010101010101010101011434805070381f402b20750458c210000018000000fd0e302d505043010a20202020202000000010000a202020202020202020202020000000fc00544c3135365644585030310a2001d67013790000030114630401847f074f002a001f0037041e00160004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000df90";
          HDMI-1 = "00ffffffffffff0010ac48f04c4a56470619010380342078ea1df5ae4f35b3250d5054a54b008180a940d100714f0101010101010101283c80a070b023403020360006442100001a000000ff00595947434e35323247564a4c0a000000fc0044454c4c2055323431330a2020000000fd00384c1e5111000a2020202020200163020325f15090050403020716011f1213142015110623091f0767030c001000382d83010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e960006442100001800000000000000000000000000000000000016";
          DP-1-1 = "00ffffffffffff0010ac2ca0533836310e12010380342078eab325ac5130b426105054a54b008180a940714f01010101010101010101283c80a070b023403020360007442100001a000000ff004a55343336383356313638530a000000fc0044454c4c20323430385746500a000000fd00384c1e5311000a202020202020012002031bf14890050403020716012309070765030c00100083010000023a801871382d40582c450007442100001e011d8018711c1620582c250007442100009e011d007251d01e206e28550007442100001e8c0ad08a20e02d10103e96000744210000180000000000000000000000000000000000000000000000000000000047";
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "1000x1200";
            mode = "1920x1080";
          };
          HDMI-1 = {
            enable = true;
            position = "1920x0";
            mode = "1920x1200";
          };
          DP-1-1 = {
            enable = true;
            position = "0x0";
            mode = "1920x1200";
          };
        };
        hooks.postswitch = "xmonad --restart; ~/.fehbg-stylix;";
      };
  };
}
