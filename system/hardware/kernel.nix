{ config, lib, pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.extraModulePackages = with config.boot.kernelPackages; [
    zenpower
  ];
}
