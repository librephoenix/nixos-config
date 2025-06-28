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
      krita
      pinta
      inkscape
      libresprite
      upscayl
    ];
    userSettings.blender.enable = true;
    xdg.mimeApps.defaultApplications = lib.mkForce {
      "image/svg+xml" = ["org.inkscape.Inkscape.desktop"];
    };
  };
}
