{ config, lib, pkgs, ...}:

let
  cfg = config.userSettings.ai;
in {
  options = {
    userSettings.ai = {
      enable = lib.mkEnableOption "Enable localai";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.local-ai ];
  };
}
