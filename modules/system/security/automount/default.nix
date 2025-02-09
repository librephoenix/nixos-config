{ config, lib, ... }:

let
  cfg = config.systemSettings.security.automount;
in {
  options = {
    systemSettings.security.automount = {
      enable = lib.mkEnableOption "Enable automount";
    };
  };

  config = lib.mkIf cfg.enable {
    services.devmon.enable = true;
    services.gvfs.enable = true;
    services.udisks2.enable = true;
  };
}
