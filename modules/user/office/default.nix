{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.office;
in {
  options = {
    userSettings.office = {
      enable = lib.mkEnableOption "Enable my office programs";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      shared-mime-info
      nautilus
      libreoffice-still
      mate.atril
      xournalpp
      adwaita-icon-theme
      newsflash
      foliate
      gnome-maps
      seahorse
      element-desktop
      openvpn
    ];
    services.syncthing.enable = true;
    services.nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
  };
}
