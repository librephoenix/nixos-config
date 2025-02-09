{ config, lib, inputs, ... }:

let
  blocklist = builtins.readFile "${inputs.blocklist-hosts}/alternates/gambling-porn/hosts";
  cfg = config.systemSettings.security.blocklist;
in {
  options = {
    systemSettings.security.blocklist = {
      enable = lib.mkEnableOption "Enable basic host blocking for bad websites";
    };
  };

  config = lib.mkIf cfg.enable {
    networking.extraHosts = ''
      "${blocklist}"
    '';
  };
}
