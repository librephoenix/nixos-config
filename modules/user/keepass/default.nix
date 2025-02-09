{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.keepass;
in {
  options = {
    userSettings.keepass = {
      enable = lib.mkEnableOption "Enable keepass password manager";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      keepassxc
      keepmenu
    ];
  };
}
