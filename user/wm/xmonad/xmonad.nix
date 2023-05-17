{ config, pkgs, ... }:

{

  imports = [ ../picom/picom.nix ];

  home.file.".config/xmonad/xmonad.hs".source = ./xmonad.hs;
  home.file.".config/xmobar/base-xmobarrc".source = ./base-xmobarrc;
  home.file.".config/xmonad/startup.sh".source = ./startup.sh;

  home.file.".config/xmonad/lib/Colors/Stylix.hs".source = config.lib.stylix.colors {
    template = builtins.readFile ./lib/Colors/Stylix.hs.mustache;
    extension = ".hs";
  };

  home.packages = with pkgs; [
    xmobar
    dunst
  ];
}
