{ config, pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.consoleLogLevel = 0;
  boot.extraModulePackages = with config.boot.kernelPackages; [
    zenpower
  ];
}
