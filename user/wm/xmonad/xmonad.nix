{ config, pkgs, ... }:

{

  imports = [ ../picom/picom.nix ];

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

  home.packages = with pkgs; [
    xmobar
    dunst
  ];
}
