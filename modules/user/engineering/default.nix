{
  config,
  lib,
  pkgs,
  pkgs-stable,
  ...
}:

let
  cfg = config.userSettings.engineering;
in
{
  options = {
    userSettings.engineering = {
      enable = lib.mkEnableOption "Enable engineering programs";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs-stable; [
      freecad
      openscad
      kicad
      cura-appimage
      arduino-ide
      graphviz
    ];
    userSettings.vscodium.enable = true;
    xdg.desktopEntries.cura = lib.mkForce {
      name = "Ultimaker Cura";
      genericName = "3D Printing Software";
      icon = "cura-icon";
      exec = "cura -platformtheme gtk3 %u";
      mimeType = [
        "model/stl"
        "application/vnd.ms-3mfdocument"
        "application/prs.wavefront-obj"
        "image/bmp"
        "image/gif"
        "image/jpeg"
        "image/png"
        "text/x-gcode"
        "application/x-amf"
        "application/x-ply"
        "application/x-ctm"
        "model/vnd.collada+xml"
        "model/gltf-binary"
        "model/gltf+json"
        "model/vnd.collada+xml+zip"
      ];
      terminal = false;
      type = "Application";
      prefersNonDefaultGPU = true;
    };
  };
}
