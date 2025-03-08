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
      virtualization = {
        docker.enable = true;
        virtualMachines.enable = false;
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
        theme = "ayu-dark";
      };
    };

    users.users.emmet.description = "Emmet";
    home-manager.users.emmet.userSettings = {
      name = "Emmet";
      email = "emmet@librephoenix.com";
    };

  };
  
}
