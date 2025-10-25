{ config, lib, pkgs, ... }:

let
  editor = config.userSettings.editor;
  term = config.userSettings.terminal;
in {
  options = {
    userSettings.editor = lib.mkOption {
      default = "vscodium";
      description = "Default editor";
      type = lib.types.enum [ "emacs" "kate" "vscodium" "zed"];
      # TODO add more editors
      #type = lib.types.enum [ "emacs" "vim" "nvim" "neovide" "nano" "micro" "vscodium" "kate" "pulsar" ];
    };
    userSettings.spawnEditor = lib.mkOption {
      default = "";
      description = "Command to spawn editor";
    };
  };

  config = {
    userSettings.zed.enable = lib.mkIf (config.userSettings.editor == "zed") true;
    userSettings.emacs.enable = lib.mkIf (config.userSettings.editor == "emacs") true;
    userSettings.vscodium.enable = lib.mkIf (config.userSettings.editor == "vscodium") true;
    home.packages = with pkgs;
      lib.optionals (editor == "kate") [ kdePackages.kate];
    userSettings.spawnEditor = lib.mkMerge [
      (lib.mkIf (editor == "emacs") "emacsclient -c -n -a 'emacs'")
      (lib.mkIf (editor == "neovide") "neovide -- --listen /tmp/nvimsocket")
      (lib.mkIf (editor == "vscodium") "codium -n")
      (lib.mkIf (editor == "zed") "zeditor")
      (lib.mkIf (builtins.elem editor [ "vim" "nvim" "nano" "micro" ]) ("exec " + term + " -e " + editor))
      (lib.mkIf (!(builtins.elem editor [ "emacs" "vim" "nvim" "neovide" "nano" "micro" "vscodium" "zed"])) editor)
    ];
    home.sessionVariables = {
      EDITOR =
        lib.mkMerge [
          (lib.mkIf (editor == "emacs") "emacsclient -c")
          (lib.mkIf (editor == "neovide") "neovide -- --listen /tmp/nvimsocket")
          (lib.mkIf (editor == "vscodium") "codium -n")
          (lib.mkIf (editor == "zed") "zeditor --wait")
          (lib.mkIf (builtins.elem editor [ "vim" "nvim" "nano" "micro" ]) ("exec " + term + " -e " + editor))
          (lib.mkIf (!(builtins.elem editor [ "emacs" "vim" "nvim" "neovide" "nano" "micro" "vscodium" "zed" ])) editor)
        ];
    };
  };
}
