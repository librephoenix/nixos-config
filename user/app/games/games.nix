{ config, pkgs, lib, ... }:
let
  myRetroarch =
    (pkgs.retroarch.override {
      cores = with pkgs.libretro; [
        mgba
        desmume
        dolphin
        citra
        genesis-plus-gx
      ];
    });
in
{
  home.packages = with pkgs; [
    # Games
    gamehub
    myRetroarch
    airshipper
    qjoypad
    superTux
    superTuxKart
  ];

  nixpkgs.config = {
    allowUnfree = true;
    allowUnfreePredicate = (_: true);
  };

  # The following 2 declarations allow retroarch to be imported into gamehub
  # Set retroarch core directory to ~/.local/bin/libretro
  # and retroarch core info directory to ~/.local/share/libretro/info
  home.file.".local/bin/libretro".source = "${myRetroarch}/lib/retroarch/cores";
  home.file.".local/share/libretro/info".source = fetchTarball {
    url = "https://github.com/libretro/libretro-core-info/archive/refs/tags/v1.15.0.tar.gz";
    sha256 = "004kgbsgbk7hn1v01jg3vj4b6dfb2cp3kcp5hgjyl030wqg1r22q";
  };
  # To get steam to import into gamehub, first install it as a flatpak, then
  # Set steam directory to ~/.var/app/com.valvesoftware.Steam/.steam
}
