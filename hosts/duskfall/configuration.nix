{ config, lib, pkgs, ... }:

{
  config = {
    systemSettings = {
      # users
      users = [ "emmet" "ignatius" ];
      adminUsers = [ "emmet" ];

      # hardware
      cachy.enable = true;
      bluetooth.enable = true;
      tlp.enable = false;
      printing.enable = true;

      # software
      flatpak.enable = false;
      virtualization = {
        docker.enable = true;
        virtualMachines.enable = false;
      };

      # wm
      hyprland.enable = true;
      gnome.enable = true;

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

    users.users.emmet.description = "Emmet";
    home-manager.users.emmet.userSettings = {
      name = "Emmet";
      email = "emmet@librephoenix.com";
    };
    users.users.ignatius.description = "Ignatius";
    home-manager.users.ignatius.userSettings = {
      stylix.theme = "woodland";
    };

    environment.systemPackages = with pkgs; [
      libwacom
    ];
    services.xserver.wacom.enable = true;
    services.xserver.videoDrivers = [ "i915" ];

    services.displayManager.defaultSession = "gnome";

  };
  
}
