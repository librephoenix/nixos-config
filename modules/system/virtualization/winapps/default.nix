{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.systemSettings.virtualization.winapps;
in {
  options = {
    systemSettings.virtualization.winapps = {
      enable = lib.mkEnableOption "Enable winapps";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with inputs.winapps.packages."${pkgs.system}"; [
      winapps
      winapps-launcher
    ] ++ [ pkgs.freerdp ];
  };
}
