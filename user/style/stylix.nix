{ config, pkgs, ... }:

{
  stylix.autoEnable = false;
  stylix.image = pkgs.fetchurl {
    url = "https://w.wallhaven.cc/full/6d/wallhaven-6d5k6x.jpg";
    sha256 = "+xl4H3UiVmMRNvMhIlaLdVTYYqnSyCTSX2UOTGsDQ8c=";
  };
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/tokyo-night-dark.yaml";
  stylix.targets.alacritty.enable = true;
  #programs.alacritty.enable = true;
  stylix.targets.kitty.enable = true;
  #programs.kitty.enable = true;
  stylix.targets.gtk.enable = true;
  stylix.targets.rofi.enable = true;
  programs.rofi.enable = true;
  stylix.targets.feh.enable = true;
  programs.feh.enable = true;
  # stylix.targets.lightdm.enable = true;
}
