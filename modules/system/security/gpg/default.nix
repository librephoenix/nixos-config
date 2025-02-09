{ lib, config, ... }:

let
  cfg = config.systemSettings.security.gpg;
in {
  options = {
    systemSettings.security.gpg = {
      enable = lib.mkEnableOption "Enable gpg";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
}
