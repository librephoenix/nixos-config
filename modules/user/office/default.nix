{
  config,
  lib,
  pkgs,
  pkgs-stable,
  ...
}:

let
  cfg = config.userSettings.office;
in
{
  options = {
    userSettings.office = {
      enable = lib.mkEnableOption "Enable my office programs";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      shared-mime-info
      kdePackages.dolphin
      libreoffice-still
      mate.atril
      xournalpp
      adwaita-icon-theme
      foliate
      gnome-maps
      seahorse
      element-desktop
      discord
      openvpn
      pomodoro-gtk
      hunspell
      hunspellDicts.en_US
    ];
    services.syncthing.enable = true;
    services.nextcloud-client = {
      package = pkgs-stable.nextcloud-client;
      enable = true;
      startInBackground = true;
    };
  };
}
