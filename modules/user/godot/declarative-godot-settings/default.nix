{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.godot;
in
{
  config = lib.mkIf cfg.enable {
    home.file.".config/declarative-godot-settings/apply_settings.gd".source = config.lib.stylix.colors {
      template = builtins.readFile ./apply_settings.gd;
      extension = ".gd";
    };
    home.file.".config/declarative-godot-settings/apply_settings.gd.uid".source = ./apply_settings.gd.uid;
    home.file.".config/declarative-godot-settings/project.godot".source = ./project.godot;
    home.file.".config/declarative-godot-settings/dummy_scene.tscn".source = ./dummy_scene.tscn;

    home.packages = [
      (pkgs.writeScriptBin "declarative-godot-settings" ''
        #!/bin/sh
        ${pkgs.godot}/bin/godot --editor --path ${config.home.homeDirectory}/.config/declarative-godot-settings;
      '')
    ];
  };
}
