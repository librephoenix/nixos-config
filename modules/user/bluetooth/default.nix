{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.bluetooth;
in {
  options = {
    userSettings.bluetooth = {
      enable = lib.mkEnableOption "Enable bluetooth";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      blueman
    ];
  };
}
