{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ virt-manager virtualbox ];
  virtualisation.libvirtd = {
    allowedBridges = [
      "nm-bridge"
      "virbr0"
    ];
    enable = true;
    qemuRunAsRoot = false;
  };
  boot.extraModulePackages = with config.boot.kernelPackages; [ virtualbox ];
}
