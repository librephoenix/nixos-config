{ lib, pkgs, inputs, userSettings, ... }:

let
  theme = import (./. + "../../../themes"+("/"+userSettings.theme));
  myLightDMTheme = if theme.polarity == "light" then "Adwaita" else "Adwaita-dark";
in
{
  imports = [ inputs.stylix.nixosModules.stylix ];

  stylix.autoEnable = false;
  stylix.polarity = theme.polarity;
  stylix.image = pkgs.fetchurl {
   url = theme.backgroundUrl;
   sha256 = theme.backgroundSha256;
  };
  stylix.base16Scheme = ./. + theme;
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
