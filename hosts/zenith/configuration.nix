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
      tlp.enable = false; # intel doesn't work with tlp
      printing.enable = true;

      # software
      flatpak.enable = false;
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
        theme = "alph";
      };
    };

    home-manager.users.emmet.userSettings = {
      name = "Emmet";
    };

    environment.systemPackages = with pkgs; [
      libwacom
    ];
    services.xserver.wacom.enable = true;
    services.xserver.videoDrivers = [ "i915" ];

    services.displayManager.defaultSession = "hyprland";

  };
  
}
