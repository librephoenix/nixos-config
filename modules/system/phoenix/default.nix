{ config, lib, pkgs, ... }:

{
  options = {
    systemSettings.dotfilesDir = lib.mkOption {
      default = "/etc/nixos";
      description = "Absolute path to the dotfiles directory";
      type = lib.types.path;
    };
  };
# TODO disabled for debugging
#  config = {
#    environment.systemPackages = [
#      # TODO update script for config schema change
#      (pkgs.writeScriptBin "phoenix" ''
#        if [ "$1" = "sync" ]; then
#          if [ "$#" = 1 ]; then
#            ''+config.systemSettings.dotfilesDir+''/scripts/sync.sh;
#            exit 0;
#          elif [ "$2" = "user" ]; then
#            ''+config.systemSettings.dotfilesDir+''/scripts/sync-user.sh;
#            exit 0;
#          elif [ "$2" = "system" ]; then
#            ''+config.systemSettings.dotfilesDir+''/scripts/sync-system.sh;
#            exit 0;
#          else
#            echo "Please pass 'system' or 'user' if supplying a second argument"
#          fi
#        elif [ "$1" = "refresh" ]; then
#          if [ "$#" -gt 1 ]; then
#            echo "Warning: The 'refresh' command has no subcommands (no $2 subcommand)";
#          fi
#          ''+config.systemSettings.dotfilesDir+''/scripts/sync-posthook.sh;
#          exit 0;
#        elif [ "$1" = "update" ]; then
#          ''+config.systemSettings.dotfilesDir+''/scripts/update.sh "''${@:2}";
#          exit 0;
#        elif [ "$1" = "upgrade" ]; then
#          if [ "$#" -gt 1 ]; then
#            echo "Warning: The 'upgrade' command has no subcommands (no $2 subcommand)";
#          fi
#          ''+config.systemSettings.dotfilesDir+''/scripts/upgrade.sh;
#          exit 0;
#        elif [ "$1" = "pull" ]; then
#          if [ "$#" -gt 1 ]; then
#            echo "Warning: The 'pull' command has no subcommands (no $2 subcommand)";
#          fi
#          ''+config.systemSettings.dotfilesDir+''/scripts/pull.sh;
#          exit 0;
#        elif [ "$1" = "harden" ]; then
#          if [ "$#" -gt 1 ]; then
#            echo "Warning: The 'harden' command has no subcommands (no $2 subcommand)";
#          fi
#          ''+config.systemSettings.dotfilesDir+''/scripts/harden.sh;
#          exit 0;
#        elif [ "$1" = "soften" ]; then
#          if [ "$#" -gt 1 ]; then
#            echo "Warning: The 'soften' command has no subcommands (no $2 subcommand)";
#          fi
#          ''+config.systemSettings.dotfilesDir+''/scripts/soften.sh;
#          exit 0;
#        elif [ "$1" = "gc" ]; then
#          if [ "$#" -gt 2 ]; then
#            echo "Warning: The 'gc' command only accepts one argument (collect_older_than)";
#          fi
#          if [ "$2" = "full" ]; then
#            sudo nix-collect-garbage --delete-old;
#            nix-collect-garbage --delete-old;
#          elif [ "$2" ]; then
#            sudo nix-collect-garbage --delete-older-than $2;
#            nix-collect-garbage --delete-older-than $2;
#          else
#            sudo nix-collect-garbage --delete-older-than 30d;
#            nix-collect-garbage --delete-older-than 30d;
#          fi
#        fi
#      '')
#    ];
#  };
}
