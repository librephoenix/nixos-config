{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.remote;
in {
  options = {
    userSettings.remote = {
      enable = lib.mkEnableOption "Enable programs for controlling remote machines";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      remmina
      sshfs
    ];
  };
}
