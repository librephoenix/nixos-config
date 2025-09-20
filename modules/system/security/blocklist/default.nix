{ config, lib, inputs, ... }:

let
  allowedHosts = config.systemSettings.security.blocklist.allowedHosts;
  hostsFileDeletions = builtins.genList (x: "0.0.0.0 " + (builtins.elemAt allowedHosts x)) (builtins.length allowedHosts);
  blocklist = builtins.replaceStrings hostsFileDeletions (builtins.genList (x: "") (builtins.length hostsFileDeletions)) (builtins.readFile "${inputs.blocklist-hosts}/alternates/gambling-porn/hosts");
  cfg = config.systemSettings.security.blocklist;
in {
  options = {
    systemSettings.security.blocklist = {
      enable = lib.mkEnableOption "Enable basic host blocking for bad websites";
      allowedHosts = lib.mkOption {
        default = [ ];
        description = "List of hosts to allow (remove from blocklist)";
        type = lib.types.listOf lib.types.str;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    networking.extraHosts = ''
      "${blocklist}"
    '';
  };
}
