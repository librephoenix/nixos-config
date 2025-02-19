{ config, lib, pkgs, inputs, ... }:

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
      mpv mpc
      yt-dlp_git
      ffmpeg
    ];

    services.mpd = rec {
      enable = true;
      musicDirectory = config.xdg.userDirs.music+"/Songs";
      playlistDirectory = config.xdg.userDirs.music+"/Playlists";
      dbFile = musicDirectory+"/mpd.db";
      extraConfig = ''
      audio_output {
          type            "pipewire"
          name            "PipeWire Sound Server"
      }
      '';
    };

    programs.ncmpcpp = {
      package = (pkgs.ncmpcpp.override { visualizerSupport = true; });
      enable = true;
      bindings = [
        { key = "j"; command = "scroll_down"; }
        { key = "k"; command = "scroll_up"; }
        { key = "J"; command = [ "select_item" "scroll_down" ]; }
        { key = "K"; command = [ "select_item" "scroll_up" ]; }
      ];
    };

  };
}
