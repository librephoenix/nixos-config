{ config, lib, pkgs, username, email, dotfilesDir, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = username;
  home.homeDirectory = "/home/"+username;

  programs.home-manager.enable = true;

  imports = [
              ../../user/shell/sh.nix # My zsh and bash config
              ../../user/shell/cli-collection.nix # Useful CLI apps
              ../../user/bin/phoenix.nix # My nix command wrapper
              ../../user/app/ranger/ranger.nix # My ranger file manager config
              ../../user/app/git/git.nix # My git config
            ];

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = with pkgs; [
    # Core
    zsh
    git
  ];

}
