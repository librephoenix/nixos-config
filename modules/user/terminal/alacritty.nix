{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.alacritty;
in {
  options = {
    userSettings.alacritty = {
      enable = lib.mkEnableOption "Enable alacritty";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.alacritty ];
    programs.alacritty.enable = true;
    programs.alacritty.settings = {
      window.opacity = lib.mkForce 0.85;
    };
  };
}
