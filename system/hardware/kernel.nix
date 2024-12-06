{ config, pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_cachyos;
  boot.consoleLogLevel = 0;
}
