{ config, lib, ... }:

let
  browser = config.userSettings.browser;
in {
  options = {
    userSettings.browser = lib.mkOption {
      default = null;
      description = "Default browser";
      type = lib.types.enum [ "brave" "qutebrowser" "librewolf" null ];
    };
    userSettings.spawnBrowser = lib.mkOption {
      default = "";
      description = "Default browser spawn command";
      type = lib.types.str;
    };
  };

  config = {
    userSettings.brave.enable = lib.mkIf (browser == "brave") true;
    userSettings.librewolf.enable = lib.mkIf (browser == "librewolf") true;
    userSettings.qutebrowser.enable = lib.mkIf (browser == "qutebrowser") true;

    userSettings.spawnBrowser = lib.mkMerge [
      (lib.mkIf (browser == "brave") "sh -c 'brave --new-window'")
      (lib.mkIf (browser == "librewolf") browser)
      (lib.mkIf (!(config.userSettings.hyprland.hyprprofiles.enable) && (browser == "qutebrowser")) "qutebrowser --qt-flag ignore-gpu-blacklist --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag enable-accelerated-2d-canvas --qt-flag num-raster-threads=4")
      (lib.mkIf (config.userSettings.hyprland.hyprprofiles.enable && (browser == "qutebrowser")) "qutebrowser-hyprprofile")
      (lib.mkIf (browser == null) "")
    ];
  };
}
