{ config, pkgs, ... }:
let
  myFont = "Inconsolata";
  myFontPkg = pkgs.inconsolata;
in
{
  #stylix.autoEnable = false;
  #stylix.polarity = "dark";
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/tokyo-dark-night.yaml";
  #stylix.fonts = {
  #  monospace = {
  #    name = myFont;
  #    package = myFontPkg;
  #  };
  #  serif = {
  #    name = myFont;
  #    package = myFontPkg;
  #  };
  #  sansSerif = {
  #    name = myFont;
  #    package = myFontPkg;
  #  };
  #  emoji = {
  #    name = "Noto Color Emoji";
  #    package = pkgs.noto-fonts-emoji-blob-bin;
  #  };
  #};
#
 # stylix.targets.grub.enable = true;
 # stylix.targets.lightdm.enable = true;
 # stylix.targets.console.enable = true;

}
