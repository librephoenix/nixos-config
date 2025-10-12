{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.godot;
in {
  options = {
    userSettings.godot = {
      enable = lib.mkEnableOption "Enable godot";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      godot_4
    ];

  };
}
