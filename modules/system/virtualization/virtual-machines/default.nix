{ lib, config, pkgs, ... }:

let cfg = config.systemSettings.virtualization.virtualMachines;
in {
  options = {
    systemSettings.virtualization.virtualMachines = {
      enable = lib.mkEnableOption "Enable qemu virtual machines, distrobox, and waydroid";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ virt-manager distrobox ];
    virtualisation.libvirtd = {
      allowedBridges = [
        "nm-bridge"
        "virbr0"
      ];
      enable = true;
      qemu.runAsRoot = false;
    };
    virtualisation.waydroid.enable = true;
  };
}
