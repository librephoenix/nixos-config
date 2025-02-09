{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.virtualization.virtualMachines;
in {
  options = {
    userSettings.virtualization.virtualMachines = {
      enable = lib.mkEnableOption "Enable helpful VM apps";
    };
  };

  config = lib.mkIf cfg.enable {
    # Various packages related to virtualization, compatability and sandboxing
    home.packages = with pkgs; [
      # Virtual Machines and wine
      libvirt
      virt-manager
      qemu
      uefi-run
      lxc
      swtpm
      bottles

      # Filesystems
      dosfstools
    ];

    home.file.".config/libvirt/qemu.conf".text = ''
      nvram = ["/run/libvirt/nix-ovmf/OVMF_CODE.fd:/run/libvirt/nix-ovmf/OVMF_VARS.fd"]
    '';
  };
}
