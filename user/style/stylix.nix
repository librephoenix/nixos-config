{ config, pkgs, myTheme, myThemePolarity, myBackgroundUrl, myBackgroundSha256, ... }:

let
  myFont = "Inconsolata";
  myFontPkg = pkgs.inconsolata;
  myThemePath = "../../../themes/"+myTheme+"/"+myTheme+".yaml";
in
{
  home.file.".currenttheme".text = myTheme;
  stylix.autoEnable = false;
  stylix.polarity = myThemePolarity;
  stylix.image = pkgs.fetchurl {
    url = myBackgroundUrl;
    sha256 = myBackgroundSha256;
  };
  stylix.base16Scheme = ./. + myThemePath;

  stylix.fonts = {
    monospace = {
      name = myFont;
      package = myFontPkg;
    };
    serif = {
      name = myFont;
      package = myFontPkg;
    };
    sansSerif = {
      name = myFont;
      package = myFontPkg;
    };
    emoji = {
      name = "Noto Color Emoji";
      package = pkgs.noto-fonts-emoji-blob-bin;
    };
    sizes = {
      terminal = 18;
      applications = 14;
      popups = 12;
      desktop = 12;
    };
  };

  stylix.targets.alacritty.enable = true;
  stylix.targets.kitty.enable = true;
  stylix.targets.gtk.enable = true;
  stylix.targets.rofi.enable = true;
  programs.rofi.enable = true;
  stylix.targets.feh.enable = true;
  programs.feh.enable = true;
  home.file.".fehbg-stylix".text = ''
    #!/bin/sh
    feh --no-fehbg --bg-fill ''+
  pkgs.fetchurl {
    url = myBackgroundUrl;
    sha256 = myBackgroundSha256;
  }+'';
  '';
  home.file.".fehbg-stylix".executable = true;
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
  home.packages = with pkgs; [
     qt5ct pkgs.libsForQt5.breeze-qt5
  ];
  home.sessionVariables = {
    QT_QPA_PLATFORMTHEME="qt5ct";
  };
  qt = {
    enable = true;
    style.package = pkgs.libsForQt5.breeze-qt5;
    style.name = "breeze-dark";
  };
}
