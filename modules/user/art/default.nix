{
  config,
  lib,
  pkgs-stable,
  ...
}:

let
  cfg = config.userSettings.art;
in
{
  options = {
    userSettings.art = {
      enable = lib.mkEnableOption "Enable art apps";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs-stable; [
      krita
      pinta
      inkscape
      libresprite
      upscayl
      material-maker
      google-fonts
      fontforge-gtk
    ];
    userSettings.blender.enable = true;
    xdg.mimeApps.defaultApplications = lib.mkForce {
      "image/svg+xml" = [ "org.inkscape.Inkscape.desktop" ];
    };
  };
}
