{ config, lib, pkgs, ...}:

let
  cfg = config.userSettings.yazi;
in {
  options = {
    userSettings.yazi = {
      enable = lib.mkEnableOption "Enable yazi TUI file manager";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.yazi = {
      enable = true;
      enableZshIntegration = true;
      keymap.manager.prepend_keymap = 
        [
          { run = "shell ' \"$@\"' --cursor=0 --interactive"; on = [ "@" ]; }
        ];
    };
  };
}
