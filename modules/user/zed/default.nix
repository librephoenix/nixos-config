{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.zed;
in {
  options = {
    userSettings.zed = {
      enable = lib.mkEnableOption "Enable zed editor";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zed-editor.enable = true;
    programs.zed-editor.extensions = [
      "nix"
      "gdscript"
    ];
  };
}
