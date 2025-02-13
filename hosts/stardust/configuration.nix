{ config, lib, pkgs, ... }:

{
  config = {
    systemSettings = {
      # users
      users = [ "corrina" ];
      adminUsers = [ "corrina" ];

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
      };

      # wm
      plasma.enable = true;

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
        theme = "spaceduck";
      };
    };

    users.users.corrina.description = "Corrina";
    home-manager.users.corrina.userSettings = {
      name = "Corrina";
    };

  };
  
}
