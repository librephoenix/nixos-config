{ config, lib, pkgs, stylix, theme, font, fontPkg, ... }:

let
  themePath = "../../../themes/"+theme+"/"+theme+".yaml";
  themePolarity = lib.removeSuffix "\n" (builtins.readFile (./. + "../../../themes"+("/"+theme)+"/polarity.txt"));
  myLightDMTheme = if themePolarity == "light" then "Adwaita" else "Adwaita-dark";
  backgroundUrl = builtins.readFile (./. + "../../../themes"+("/"+theme)+"/backgroundurl.txt");
  backgroundSha256 = builtins.readFile (./. + "../../../themes/"+("/"+theme)+"/backgroundsha256.txt");
in
{
  imports = [ stylix.nixosModules.stylix ];

  stylix.autoEnable = false;
  stylix.polarity = themePolarity;
  stylix.image = pkgs.fetchurl {
   url = backgroundUrl;
   sha256 = backgroundSha256;
  };
  stylix.base16Scheme = ./. + themePath;
  stylix.fonts = {
    monospace = {
      name = font;
      package = fontPkg;
    };
    serif = {
      name = font;
      package = fontPkg;
    };
    sansSerif = {
      name = font;
      package = fontPkg;
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

  environment.sessionVariables = {
    QT_QPA_PLATFORMTHEME = "qt5ct";
  };

}
