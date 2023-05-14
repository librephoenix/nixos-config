{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Games
    #TODO need flatpak steam
    gamehub
    retroarch
    libretro.mgba
    libretro.desmume
    libretro.dolphin
    libretro.citra
    libretro.genesis-plus-gx
    airshipper
    qjoypad
    # TODO need flatpak minecraft
  ];
}
