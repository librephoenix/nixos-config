{ config, lib, pkgs, ... }:

let
  cfg = config.systemSettings.security.openvpn;
in {
  options = {
    systemSettings.security.openvpn = {
      enable = lib.mkEnableOption "Enable openvpn";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.openvpn ];
    environment.etc.openvpn.source = "${pkgs.update-resolv-conf}/libexec/openvpn";
  };
}
