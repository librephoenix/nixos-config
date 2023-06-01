{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Games
    #TODO need flatpak steam
    gamehub
    (retroarch.override {
      cores = with libretro; [
        mgba
        desmume
        dolphin
        citra
        genesis-plus-gx
      ];
    })
    airshipper
    qjoypad
    # TODO need flatpak minecraft
  ];
}
