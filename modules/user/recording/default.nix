{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.recording;
in {
  options = {
    userSettings.recording = {
      enable = lib.mkEnableOption "Enable studio recording and editing programs";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      obs-studio
      kdePackages.kdenlive
      tenacity
      ardour
    ];
  };
}
