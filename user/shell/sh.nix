{ config, pkgs, ... }:
let

  # My shell aliases
  myAliases = {
    ls = "exa --icons -l -T -L=1";
    cat = "bat";
    htop = "btm";
    fd = "fd -Lu";
    w3m = "w3m -no-cookie -v";
  };
in
{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    shellAliases = myAliases;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "agnoster";
    };
  };

  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = myAliases;
  };
}
