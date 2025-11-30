{ pkgs, ... }:

{
  config = {
    systemSettings = {
      # users
      users = [ "emmet" ];
      adminUsers = [ "emmet" ];

      # hardware
      cachy.enable = true;
      cachy.variant = "lts";
      bluetooth.enable = true;
      powerprofiles.enable = true;
      tlp.enable = false;
      printing.enable = true;

      # software
      flatpak.enable = true;
      gaming.enable = true;
      virtualization = {
        docker.enable = true;
        virtualMachines.enable = true;
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
        theme = "orichalcum";
      };
    };

    services.thermald.enable = true;
    services.asusd.enable = true;
    services.supergfxd.enable = true;
    environment.systemPackages = with pkgs; [
      asusctl
      supergfxctl
    ];

    users.users.emmet.description = "Emmet";
    home-manager.users.emmet.userSettings = {
      name = "Emmet";
      email = "emmet@librephoenix.com";
    };

    services.displayManager.defaultSession = "hyprland";

  };

}
