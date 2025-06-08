{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.shell.apps;
in {
  options = {
    userSettings.shell.apps = {
      enable = lib.mkEnableOption "Enable a collection of additional useful CLI apps";
    };
  };

  config = lib.mkIf cfg.enable {
    # Collection of useful CLI apps
    home.packages = with pkgs; [
      # Command Line
      killall
      libnotify
      timer
      brightnessctl
      gnugrep
      bat eza fd bottom ripgrep
      rsync
      zip unzip
      w3m
      pandoc
      hwinfo
      pciutils
      numbat
      (pkgs.writeShellScriptBin "airplane-mode" ''
        #!/bin/sh
        connectivity="$(nmcli n connectivity)"
        if [ "$connectivity" == "full" ]
        then
            nmcli n off
        else
            nmcli n on
        fi
      '')
      (pkgs.writeScriptBin "comma" ''
        if [ "$#" = 0 ]; then
          echo "usage: comma PKGNAME... [EXECUTABLE]";
        elif [ "$#" = 1 ]; then
          nix-shell -p $1 --run $1;
        elif [ "$#" = 2 ]; then
          nix-shell -p $1 --run $2;
        else
          echo "error: too many arguments";
          echo "usage: comma PKGNAME... [EXECUTABLE]";
        fi
      '')
      (pkgs.writeScriptBin "comma-shell" ''
        if [ "$#" = 0 ]; then
          echo "usage: comma-shell PKGNAME1 [PKGNAME2 PKGNAME3...]";
        else
          nix-shell -p $@
        fi
      '')
    ];

    programs.zsh.shellAliases = {
      w3m = "w3m -no-cookie -v";
      "," = "comma";
      ",," = "comma-shell";
    };
  };
}
