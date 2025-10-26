{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.media;
in {
  options = {
    userSettings.media = {
      enable = lib.mkEnableOption "Enable media playback apps";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      vlc
      mpv
      yt-dlp_git
      ffmpeg
    ];

  };
}
