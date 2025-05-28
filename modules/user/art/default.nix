{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.art;
in {
  options = {
    userSettings.art = {
      enable = lib.mkEnableOption "Enable art apps";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      gimp
      krita
      pinta
      inkscape
      libresprite
      makehuman
      upscayl
    ];
    userSettings.blender.enable = true;
  };
}
