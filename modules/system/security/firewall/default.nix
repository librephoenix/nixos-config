{ config, lib, ... }:

let
  cfg = config.systemSettings.security.firewall;
in {
  options = {
    systemSettings.security.firewall = {
      # TODO make this more granular and better :|
      enable = lib.mkEnableOption "Actvate firewall with ports open only for syncthing";
    };
  };

  config = lib.mkIf cfg.enable {
    # Firewall
    networking.firewall.enable = true;
    # Open ports in the firewall.
    networking.firewall.allowedTCPPorts = [ 22000 21027 ]; # syncthing
    networking.firewall.allowedUDPPorts = [ 22000 21027 ]; # syncthing
    # Or disable the firewall altogether.
    # networking.firewall.enable = false;
  };
}
