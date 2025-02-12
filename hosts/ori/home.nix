{ config, lib, pkgs, ... }:

{
  config = {

    userSettings = {
      # setup
      shell = {
        enable = true;
        apps.enable = true;
      };
      xdg.enable = true;

      # programs
      ranger.enable = true;
      git.enable = true;
    };

  };
}
