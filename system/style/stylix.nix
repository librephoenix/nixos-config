{ config, pkgs, stylix, myTheme, myThemePolarity, myBackgroundUrl, myBackgroundSha256, ... }:

let
  myFont = "Inconsolata";
  myFontPkg = pkgs.inconsolata;
  myThemePath = "../../../themes/"+myTheme+"/"+myTheme+".yaml";
  myLightDMTheme = if myThemePolarity == "light" then "Adwaita" else "Adwaita-dark";
in
{
  imports = [ stylix.nixosModules.stylix ];

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
  };

  stylix.targets.lightdm.enable = true;
  services.xserver.displayManager.lightdm = {
      greeters.slick.enable = true;
      greeters.slick.theme.name = myLightDMTheme;
  };
  stylix.targets.console.enable = true;

}
