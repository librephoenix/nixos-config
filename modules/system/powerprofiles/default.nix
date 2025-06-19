{ config, lib, ... }:

let
  cfg = config.systemSettings.powerprofiles;
in {
  options = {
    systemSettings.powerprofiles = {
      enable = lib.mkEnableOption "Enable power profiles daemon";
    };
  };

  config = lib.mkIf cfg.enable {
    services.power-profiles-daemon = {
      enable = true;
    };
  };
}
