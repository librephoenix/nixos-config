{ pkgs, lib, config, ... }:

let
  cfg = config.systemSettings.printing;
in {
  options = {
    systemSettings.printing = {
      enable = lib.mkEnableOption "Enable printing";
    };
  };

  config = lib.mkIf cfg.enable {
    # Enable printing
    services.printing.enable = true;
    services.avahi.enable = true;
    services.avahi.nssmdns4 = true;
    services.avahi.openFirewall = true;
    environment.systemPackages = [ pkgs.cups-filters ];
  };
}
