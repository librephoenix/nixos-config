{ pkgs, ... }:

{
  config = {
    systemSettings = {
      # users
      users = [ "emmet" "ignatius" ];
      adminUsers = [ "emmet" ];

      # hardware
      cachy.enable = true;
      cachy.variant = "lts";
      bluetooth.enable = true;
      powerprofiles.enable = true;
      tlp.enable = false;
      printing.enable = true;

      # software
      flatpak.enable = false;
      virtualization = {
        docker.enable = false;
        virtualMachines.enable = false;
      };
      brave.enable = true;

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

  };

}
