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
    home.packages = with pkgs; [
      gitu nixd
    ];
    stylix.targets.zed.enable = true;
    programs.zed-editor.enable = true;
    programs.zed-editor.extensions = [
      "nix"
      "gdscript"
      "git_firefly"
      "toml"
      "xml"
      "svelte"
      "vue"
      "scss"
      "make"
      "dockerfile"
      "docker-compose"
      "hyprlang"
      "java"
      "lua"
      "r"
      "kotlin"
      "haskell"
      "perl"
      "fortran"
      "ruby"
      "org"
    ];
  };
}
