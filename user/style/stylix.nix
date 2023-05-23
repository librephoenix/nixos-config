{ config, pkgs, myTheme, myBackgroundUrl, myBackgroundSha256, ... }:

let
  myFont = "Inconsolata";
  myFontPkg = pkgs.inconsolata;
in
{
  stylix.autoEnable = false;
  stylix.polarity = "dark";
  stylix.image = pkgs.fetchurl {
    url = myBackgroundUrl;
    sha256 = myBackgroundSha256;
  };
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/"+myTheme+".yaml";

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
}
