{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.shell;
in {
  options = {
    userSettings.shell = {
      enable = lib.mkEnableOption "Enable fancy zsh with some necessary CLI utilities";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      syntaxHighlighting.enable = true;
      enableCompletion = true;
      shellAliases = {
        ls = "eza --icons -l -T -L=1";
        cat = "bat";
        htop = "btm";
        fd = "fd -Lu";
        w3m = "w3m -no-cookie -v";
        "," = "comma";
        ",," = "comma-shell";
      };
      initExtra = ''
      PROMPT=" ◉ %U%F{magenta}%n%f%u@%U%F{blue}%m%f%u:%F{yellow}%~%f
       %F{green}→%f "
      RPROMPT="%F{red}▂%f%F{yellow}▄%f%F{green}▆%f%F{cyan}█%f%F{blue}▆%f%F{magenta}▄%f%F{white}▂%f"
      [ $TERM = "dumb" ] && unsetopt zle && PS1='$ '
      bindkey '^P' history-beginning-search-backward
      bindkey '^N' history-beginning-search-forward
      '';
    };

    programs.bash = {
      enable = true;
      enableCompletion = true;
      shellAliases = config.programs.zsh.shellAliases;
    };

    home.packages = with pkgs; [
      gnugrep gnused w3m
      bat eza bottom fd bc
      direnv nix-direnv
    ];

    programs.neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
    };

    programs.direnv.enable = true;
    programs.direnv.enableZshIntegration = true;
    programs.direnv.nix-direnv.enable = true;
    programs.direnv.nix-direnv.package = pkgs.nix-direnv-flakes;
  };
}
