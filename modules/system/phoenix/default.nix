{ config, lib, pkgs, ... }:

{
  options = {
    systemSettings = {
      dotfilesDir = lib.mkOption {
        default = "/etc/nixos";
        description = "Absolute path to the dotfiles directory";
        type = lib.types.path;
      };
      secretsFlakeDir = lib.mkOption {
        default = "/etc/nixos.secrets";
        description = "Absolute path to my secrets flake";
        type = lib.types.path;
      };
    };
  };
  config = {
    environment.systemPackages = with pkgs; [
      attic-client
      (pkgs.writeScriptBin "phoenix" ''
        if [[ $EUID -ne 0 ]]; then
          echo "Error: This script must be run as root" 1>&2
          exit 1
        fi
        if [ "$1" = "sync" ]; then
          if [ "$#" -gt 1 ]; then
            echo "Warning: The 'sync' command has no subcommands (no $2 subcommand)";
          fi
          chown -R 0:0 ${config.systemSettings.dotfilesDir};
          chown -R 0:0 ${config.systemSettings.secretsFlakeDir};
          pushd ${config.systemSettings.dotfilesDir} &> /dev/null;
          nixos-rebuild switch;
          popd &> /dev/null;
          exit 0;
        elif [ "$1" = "update" ]; then
          pushd ${config.systemSettings.dotfilesDir} &> /dev/null;
          nix flake update "''${@:2}";
          popd &> /dev/null;
          if [ "$#" -eq 1 ]; then
            pushd ${config.systemSettings.secretsFlakeDir} &> /dev/null;
            nix flake update;
            popd &> /dev/null;
          fi
          exit 0;
        elif [ "$1" = "pull" ]; then
          if [ "$#" -gt 1 ]; then
            echo "Warning: The 'pull' command has no subcommands (no $2 subcommand)";
          fi
          chown -R $DOAS_USER:users ${config.systemSettings.dotfilesDir};
          chown -R $DOAS_USER:users ${config.systemSettings.secretsFlakeDir};
          pushd ${config.systemSettings.dotfilesDir} &> /dev/null;
          sudo -u $DOAS_USER git stash;
          sudo -u $DOAS_USER git pull;
          sudo -u $DOAS_USER git stash apply;
          popd &> /dev/null;
          pushd ${config.systemSettings.secretsFlakeDir} &> /dev/null;
          sudo -u $DOAS_USER git stash;
          sudo -u $DOAS_USER git pull;
          sudo -u $DOAS_USER git stash apply;
          popd &> /dev/null;
          chown -R 0:0 ${config.systemSettings.dotfilesDir};
          chown -R 0:0 ${config.systemSettings.secretsFlakeDir};
          exit 0;
        # TODO allow specifying host with $2 in build subcommand
        elif [ "$1" = "build" ]; then
          if [ "$#" -gt 1 ]; then
            echo "Warning: The 'build' command has no subcommands (no $2 subcommand)";
          fi
          pushd ${config.systemSettings.dotfilesDir} &> /dev/null;
          nixos-rebuild build --flake .#snowfire;
          attic push emmet ./result;
          rm ./result;
          nixos-rebuild build --flake .#polarias;
          attic push emmet ./result;
          rm ./result;
          nixos-rebuild build --flake .#zenith;
          attic push emmet ./result;
          rm ./result;
          nixos-rebuild build --flake .#stardust;
          attic push emmet ./result;
          rm ./result;
          nixos-rebuild build --flake .#ori;
          attic push emmet ./result;
          rm ./result;
          exit 0;
        elif [ "$1" = "lock" ]; then
          if [ "$#" -gt 1 ]; then
            echo "Warning: The 'lock' command has no subcommands (no $2 subcommand)";
          fi
          chown -R 0:0 ${config.systemSettings.dotfilesDir};
          chown -R 0:0 ${config.systemSettings.secretsFlakeDir};
          exit 0;
        elif [ "$1" = "unlock" ]; then
          if [ "$#" -gt 1 ]; then
            echo "Warning: The 'unlock' command has no subcommands (no $2 subcommand)";
          fi
          chown -R $DOAS_USER:users ${config.systemSettings.dotfilesDir};
          chown -R $DOAS_USER:users ${config.systemSettings.secretsFlakeDir};
          exit 0;
        elif [ "$1" = "gc" ]; then
          if [ "$#" -gt 2 ]; then
            echo "Warning: The 'gc' command only accepts one argument (collect_older_than)";
          fi
          if [ "$2" = "full" ]; then
            nix-collect-garbage --delete-old;
          elif [ "$2" ]; then
            nix-collect-garbage --delete-older-than $2;
          else
            nix-collect-garbage --delete-older-than 30d;
          fi
          exit 0;
        fi
      '')
    ];
  };
}
