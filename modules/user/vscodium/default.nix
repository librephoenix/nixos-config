{ config, lib, pkgs, ... }:
let
  cfg = config.userSettings.vscodium;
in {
  options = {
    userSettings.vscodium = {
      enable = lib.mkEnableOption "Enable vscodium";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      profiles.default.extensions = with pkgs.vscode-extensions; [
          vscodevim.vim
          kahole.magit
          jnoortheen.nix-ide
          mkhl.direnv
          geequlim.godot-tools
          yzhang.markdown-all-in-one
      ];
      profiles.default.userSettings = {
        "keyboard.dispatch" = "keyCode";
      };
      profiles.default.keybindings = [
        {
           "key" = "g g";
           "command" = "cursorTop";
           "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/" ;
        }
        { "key" = "g r";
           "command" = "magit.refresh";
           "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/" ;
        }
        {
          "key" = "tab";
          "command" = "extension.vim_tab";
          "when" = "editorTextFocus && vim.active && !inDebugRepl && vim.mode != 'Insert' && editorLangId != 'magit'";
        }
        {
          "key" = "tab";
          "command" = "-extension.vim_tab";
        }
        {
          "key" = "x";
          "command" = "magit.discard-at-point";
          "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
        }
        {
          "key" = "k";
          "command" = "-magit.discard-at-point";
        }
        {
          "key" = "-";
          "command" = "magit.reverse-at-point";
          "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
        }
        {
          "key" = "v";
          "command" = "-magit.reverse-at-point";
        }
        {
          "key" = "shift+-";
          "command" = "magit.reverting";
          "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
        }
        {
          "key" = "shift+v";
          "command" = "-magit.reverting";
        }
        {
          "key" = "shift+o";
          "command" = "magit.resetting";
          "when" = "editorTextFocus && editorLangId == 'magit' && vim.mode =~ /^(?!SearchInProgressMode|CommandlineInProgress).*$/";
        }
        {
          "key" = "shift+x";
          "command" = "-magit.resetting";
        }
        {
          "key" = "x";
          "command" = "-magit.reset-mixed";
        }
        {
          "key" = "ctrl+u x";
          "command" = "-magit.reset-hard";
        }
      ];
      mutableExtensionsDir = false;

    };
    stylix.targets.vscode.enable = true;
    };
}
