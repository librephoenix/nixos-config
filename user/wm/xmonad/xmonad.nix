{ config, pkgs, ... }:

{
  home.file.".config/xmonad/xmonad.hs".source = ./xmonad.hs;
  home.file.".config/xmobar/base-xmobarrc".source = ./base-xmobarrc;
  home.file.".config/xmonad/startup.sh".source = ./startup.sh;
}
