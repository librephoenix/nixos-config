{ config, lib, pkgs, ... }:

let
  editor = config.userSettings.editor;
  term = config.userSettings.terminal;
in {
  options = {
    userSettings.editor = lib.mkOption {
      default = "emacs";
      description = "Default editor";
      type = lib.types.enum [ "emacs" ];
      # TODO add more editors
      #type = lib.types.enum [ "emacs" "vim" "nvim" "neovide" "nano" "micro" "vscodium" "kate" "pulsar" ];
    };
    userSettings.spawnEditor = lib.mkOption {
      default = "";
      description = "Command to spawn editor";
    };
  };

  config = {
    userSettings.emacs.enable = lib.mkIf (config.userSettings.editor == "emacs") true;
    userSettings.spawnEditor = lib.mkMerge [
      (lib.mkIf (editor == "emacs") "emacsclient -c -a 'emacs'")
      (lib.mkIf (editor == "neovide") "neovide -- --listen /tmp/nvimsocket")
      (lib.mkIf (builtins.elem editor [ "vim" "nvim" "nano" "micro" ]) ("exec " + term + " -e " + editor))
      (lib.mkIf (!(builtins.elem editor [ "emacs" "vim" "nvim" "neovide" "nano" "micro"])) editor)
    ];
  };
}
