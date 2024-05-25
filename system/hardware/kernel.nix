{ config, pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;
  boot.consoleLogLevel = 0;
}
