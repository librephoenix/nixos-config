{ config, lib, pkgs, userSettings, ... }:

let
  themePath = "../../../themes"+("/"+userSettings.theme+"/"+userSettings.theme)+".yaml";
  themePolarity = lib.removeSuffix "\n" (builtins.readFile (./. + "../../../themes"+("/"+userSettings.theme)+"/polarity.txt"));
  backgroundUrl = builtins.readFile (./. + "../../../themes"+("/"+userSettings.theme)+"/backgroundurl.txt");
  backgroundSha256 = builtins.readFile (./. + "../../../themes/"+("/"+userSettings.theme)+"/backgroundsha256.txt");
in
{
  home.file.".currenttheme".text = userSettings.theme;
  stylix.autoEnable = false;
  stylix.polarity = themePolarity;
  stylix.image = pkgs.fetchurl {
    url = backgroundUrl;
    sha256 = backgroundSha256;
  };
  stylix.base16Scheme = ./. + themePath;

  stylix.fonts = {
    monospace = {
      name = userSettings.font;
      package = userSettings.fontPkg;
    };
    serif = {
      name = userSettings.font;
      package = userSettings.fontPkg;
    };
    sansSerif = {
      name = userSettings.font;
      package = userSettings.fontPkg;
    };
    emoji = {
      name = "Noto Color Emoji";
      package = pkgs.noto-fonts-emoji-blob-bin;
    };
    sizes = {
      terminal = 18;
      applications = 12;
      popups = 12;
      desktop = 12;
    };
  };

  stylix.targets.alacritty.enable = false;
  programs.alacritty.settings = {
    colors = {
      # TODO revisit these color mappings
      # these are just the default provided from stylix
      # but declared directly due to alacritty v3.0 breakage
      primary.background = "#"+config.lib.stylix.colors.base00;
      primary.foreground = "#"+config.lib.stylix.colors.base07;
      cursor.text = "#"+config.lib.stylix.colors.base00;
      cursor.cursor = "#"+config.lib.stylix.colors.base07;
      normal.black = "#"+config.lib.stylix.colors.base00;
      normal.red = "#"+config.lib.stylix.colors.base08;
      normal.green = "#"+config.lib.stylix.colors.base0B;
      normal.yellow = "#"+config.lib.stylix.colors.base0A;
      normal.blue = "#"+config.lib.stylix.colors.base0D;
      normal.magenta = "#"+config.lib.stylix.colors.base0E;
      normal.cyan = "#"+config.lib.stylix.colors.base0B;
      normal.white = "#"+config.lib.stylix.colors.base05;
      bright.black = "#"+config.lib.stylix.colors.base03;
      bright.red = "#"+config.lib.stylix.colors.base09;
      bright.green = "#"+config.lib.stylix.colors.base01;
      bright.yellow = "#"+config.lib.stylix.colors.base02;
      bright.blue = "#"+config.lib.stylix.colors.base04;
      bright.magenta = "#"+config.lib.stylix.colors.base06;
      bright.cyan = "#"+config.lib.stylix.colors.base0F;
      bright.white = "#"+config.lib.stylix.colors.base07;
    };
    font.size = config.stylix.fonts.sizes.terminal;
  };
  stylix.targets.kitty.enable = true;
  stylix.targets.gtk.enable = true;
  stylix.targets.rofi.enable = if (userSettings.wmType == "x11") then true else false;
  stylix.targets.feh.enable = if (userSettings.wmType == "x11") then true else false;
  programs.feh.enable = true;
  home.file.".fehbg-stylix".text = ''
    #!/bin/sh
    feh --no-fehbg --bg-fill ''+config.stylix.image+'';
  '';
  home.file.".fehbg-stylix".executable = true;
  home.file.".swaybg-stylix".text = ''
    #!/bin/sh
    swaybg -m fill -i ''+config.stylix.image+'';
  '';
  home.file.".swaybg-stylix".executable = true;
  home.file.".swayidle-stylix".text = ''
    #!/bin/sh
    swaylock_cmd='swaylock --indicator-radius 200 --screenshots --effect-blur 10x10'
    swayidle -w timeout 300 "$swaylock_cmd --fade-in 0.5 --grace 5" \
                timeout 600 'hyprctl dispatch dpms off' \
                resume 'hyprctl dispatch dpms on' \
                before-sleep "$swaylock_cmd"
    '';
  home.file.".swayidle-stylix".executable = true;
  home.file = {
    ".config/qt5ct/colors/oomox-current.conf".source = config.lib.stylix.colors {
      template = builtins.readFile ./oomox-current.conf.mustache;
      extension = ".conf";
    };
    ".config/Trolltech.conf".source = config.lib.stylix.colors {
      template = builtins.readFile ./Trolltech.conf.mustache;
      extension = ".conf";
    };
    ".config/kdeglobals".source = config.lib.stylix.colors {
      template = builtins.readFile ./Trolltech.conf.mustache;
      extension = "";
    };
    ".config/qt5ct/qt5ct.conf".text = pkgs.lib.mkBefore (builtins.readFile ./qt5ct.conf);
  };
  home.file.".config/hypr/hyprpaper.conf".text = ''
    preload = ''+config.stylix.image+''

    wallpaper = eDP-1,''+config.stylix.image+''

    wallpaper = HDMI-A-1,''+config.stylix.image+''

    wallpaper = DP-1,''+config.stylix.image+''
  '';
  home.packages = with pkgs; [
     qt5ct pkgs.libsForQt5.breeze-qt5
  ];
  home.sessionVariables = {
    QT_QPA_PLATFORMTHEME="qt5ct";
  };
  programs.zsh.sessionVariables = {
    QT_QPA_PLATFORMTHEME="qt5ct";
  };
  programs.bash.sessionVariables = {
    QT_QPA_PLATFORMTHEME="qt5ct";
  };
  qt = {
    enable = true;
    style.package = pkgs.libsForQt5.breeze-qt5;
    style.name = "breeze-dark";
  };
}
