{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.flatpak;
in {
  options = {
    userSettings.flatpak = {
      enable = lib.mkEnableOption "Enable flatpak support";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.flatpak ];
    home.sessionVariables = {
      XDG_DATA_DIRS = "$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share"; # lets flatpak work
    };

    #services.flatpak.enable = true;
    #services.flatpak.packages = [ { appId = "com.kde.kdenlive"; origin = "flathub";  } ];
    #services.flatpak.update.onActivation = true;
  };
}
