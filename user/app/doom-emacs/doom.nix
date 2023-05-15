{ config, lib, pkgs, ... }:

{
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./.;
  };
  home.file.".emacs.d/themes/doom-stylix-theme.el".source = config.lib.stylix.colors {
      template = builtins.readFile ./themes/doom-stylix-theme.el.mustache;
      extension = ".el";
  };
}
