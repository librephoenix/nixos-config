{ config, lib, pkgs, ... }:

{
  config = {
    systemSettings = {
      # users
      users = [ "emmet" "corrina" ];
      adminUsers = [ "emmet" "corrina" ];

      # hardware
      cachy.enable = true;
      bluetooth.enable = true;
      tlp.enable = false;
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

    services.thermald.enable = true;

    users.users.emmet.description = "Emmet";
    home-manager.users.emmet.userSettings = {
      name = "Emmet";
      email = "emmet@librephoenix.com";
    };
    users.users.corrina.description = "Corrina";
    home-manager.users.corrina.userSettings = {
      name = "Corrina";
      email = "";
      stylix.theme = "spaceduck";
    };
    home-manager.users.corrina.services.nextcloud-client = {
      enable = lib.mkForce false;
      startInBackground = lib.mkForce false;
    };

  };
  
}
