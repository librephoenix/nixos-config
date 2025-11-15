{
  config,
  lib,
  pkgs,
  pkgs-stable,
  ...
}:

let
  cfg = config.userSettings.recording;
in
{
  options = {
    userSettings.recording = {
      enable = lib.mkEnableOption "Enable studio recording and editing programs";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.obs-studio.enable = true;
    programs.obs-studio.plugins = with pkgs.obs-studio-plugins; [
      obs-gstreamer
      obs-vaapi
      obs-scale-to-sound
    ];
    home.packages = with pkgs-stable; [
      kdePackages.kdenlive
      #tenacity
    ];
  };
}
