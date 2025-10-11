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
      theme.icon = {
        dirs = [
          { name = ".config"; text = ""; }
          { name = ".git"; text = ""; }
          { name = ".github"; text = ""; }
          { name = ".npm"; text = ""; }
          { name = "Desktop"; text = ""; }
          { name = "Development"; text = ""; }
          { name = "Documents"; text = ""; }
          { name = "Downloads"; text = ""; }
          { name = "Library"; text = ""; }
          { name = "Movies"; text = ""; }
          { name = "Music"; text = ""; }
          { name = "Pictures"; text = ""; }
          { name = "Public"; text = ""; }
          { name = "Videos"; text = ""; }
          { name = "nixos"; text = ""; }
          { name = "Archive"; text = ""; }
          { name = "Media"; text = ""; }
          { name = "Podcasts"; text = ""; }
          { name = "Drive"; text = ""; }
          { name = "KP"; text = ""; }
          { name = "Books"; text = ""; }
          { name = "Games"; text = ""; }
          { name = "Game Saves"; text = ""; }
          { name = "Templates"; text = ""; }
          { name = "Notes"; text = ""; }
          { name = "Projects"; text = ""; }
          { name = "Screenshots"; text = ""; }
        ];
      };
      keymap.manager.prepend_keymap = 
        [
	        { run = "cd ~/Projects"; on = [ "g" "p" ]; desc = "Go to projects"; }
	        { run = "cd ~/Screenshots"; on = [ "g" "s" ]; desc = "Go to screenshots"; }
          { run = "shell ' \"$@\"' --cursor=0 --interactive"; on = [ "@" ]; }
          { run = "hidden toggle"; on = [ "<C-h>" ]; }
          { run = "yank"; on = [ "y" "y" ]; }
          { run = "copy path"; on = [ "y" "p" ]; }
          { run = "copy dirname"; on = [ "y" "d" ]; }
          { run = "copy filename"; on = [ "y" "n" ]; }
          { run = "copy name_without_ext"; on = [ "y" "N" ]; }
          { run = "yank --cut"; on = [ "d" "d" ]; }
          { run = "remove --force"; on = [ "d" "D" ]; }
          { run = "paste"; on = [ "p" "p" ]; }
          { run = "paste --force"; on = [ "p" "P" ]; }
          { run = "cd --interactive"; on = [ "c" "d" ]; }
          { run = "sort mtime --reverse=no"; on = [ "o" "m" ]; }
          { run = "sort mtime --reverse=yes"; on = [ "o" "M" ]; }
          { run = "sort natural --reverse=no"; on = [ "o" "b" ]; }
          { run = "sort natural --reverse=yes"; on = [ "o" "B" ]; }
          { run = "sort alphabetical --reverse=no"; on = [ "o" "a" ]; }
          { run = "sort alphabetical --reverse=yes"; on = [ "o" "A" ]; }
          { run = "sort extension --reverse=no"; on = [ "o" "e" ]; }
          { run = "sort extension --reverse=yes"; on = [ "o" "E" ]; }
          { run = "sort size --reverse=no"; on = [ "o" "s" ]; }
          { run = "sort size --reverse=yes"; on = [ "o" "S" ]; }
          { run = "tab_create --current"; on = [ "t" ]; }
          { run = "close"; on = [ "x" ]; }
          { run = "tab_switch 1 --relative"; on = [ "J" ]; }
          { run = "tab_switch 1 --relative"; on = [ "<C-Tab>" ]; }
          { run = "tab_switch -1 --relative"; on = [ "K" ]; }
          { run = "tab_switch -1 --relative"; on = [ "<C-BackTab>" ]; }
          { run = "undo"; on = [ "u" ]; }
          { run = "redo"; on = [ "<C-r>" ]; }
        ];
      plugins = {
        full-border = pkgs.yaziPlugins.full-border;
        git = pkgs.yaziPlugins.git;
        sudo = pkgs.yaziPlugins.sudo;
        piper = pkgs.yaziPlugins.piper;
      };
      initLua = ''
        require("full-border"):setup()
        require("git"):setup()
      '';
      settings = {
        "plugin.prepend_fetchers" = [
          {
            id = "git";
            name = "*";
            run = "git";
          }
          {
            id = "git";
            name = "*/";
            run = "git";
          }
        ];
      };
    };
  };
}
