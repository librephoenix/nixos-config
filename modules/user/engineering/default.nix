{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.engineering;
in {
  options = {
    userSettings.engineering = {
      enable = lib.mkEnableOption "Enable engineering programs";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      freecad
      openscad
      kicad
      cura-appimage
      arduino-ide
    ];
  };
}
