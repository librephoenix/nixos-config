{ ... }:
{
  imports =
    [ ../work/configuration.nix # Personal is essentially work system + games
      ../../system/hardware-configuration.nix
      ../../system/app/gamemode.nix
      ../../system/app/prismlauncher.nix
    ];
}
