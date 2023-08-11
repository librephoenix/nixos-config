{ config, lib, pkgs, ... }:

{
  # Various packages related to virtualization, compatability and sandboxing
  home.packages = with pkgs; [
    # Virtual Machines and wine
    #libvirt
    #virt-manager
    #qemu_full
    #lxc
    #swtpm
    bottles

    # Filesystems
    dosfstools
  ];

}
