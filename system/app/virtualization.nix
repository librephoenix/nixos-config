{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.virt-manager ];
  virtualisation.libvirtd = {
  allowedBridges = [
    "nm-bridge"
    "virbr0"
  ];
  enable = true;
  qemuRunAsRoot = false;
  };
}
