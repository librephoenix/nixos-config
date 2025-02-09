{ config, lib, pkgs, ... }:

{
  config = {
    systemSettings = {
      # users
      users = [ "emmet" ];
      adminUsers = [ "emmet" ];

      # hardware
      cachy.enable = true;
      bluetooth.enable = true;
      tlp.enable = true;
      printing.enable = true;

      # software
      flatpak.enable = false;
      gaming.enable = true;
      virtualization = {
        docker.enable = true;
        virtualMachines.enable = true;
      };

      # wm
      hyprland.enable = true;

      # dotfiles
      dotfilesDir = "/etc/nixos";

      # security
      security = {
        automount.enable = true;
        blocklist.enable = true;
        doas.enable = true;
        firejail.enable = false; # TODO setup firejail profiles
        firewall.enable = true;
        gpg.enable = true;
        openvpn.enable = true;
        sshd.enable = false;
      };

      # style
      stylix = {
        enable = true;
        theme = "io";
      };
    };
  };
  
}
