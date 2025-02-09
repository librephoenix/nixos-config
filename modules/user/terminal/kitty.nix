{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.kitty;
in {
  options = {
    userSettings.kitty = {
      enable = lib.mkEnableOption "Enable kitty";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.kitty ];
    programs.kitty.enable = true;
    programs.kitty.settings = {
      background_opacity = lib.mkForce "0.85";
      modify_font = "cell_width 90%";
    };
  };
}
