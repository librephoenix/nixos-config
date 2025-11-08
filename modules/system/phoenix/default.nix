{
  config,
  lib,
  pkgs,
  pkgs-stable,
  ...
}:

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
      systemBuilder.enable = lib.mkEnableOption "Enable automatic config updater and builder";
      systemBuilder.buildCronExpression = lib.mkOption {
        default = "Sat *-*-* 02:00:00"; # Sat morning at 2am
        description = "Cron expression for when the system should auto build config";
        type = lib.types.str;
      };
    };
  };
  config = {
    environment.systemPackages = with pkgs; [
      attic-client
      git
      git-lfs
      nix-output-monitor
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
          systemd-inhibit --what sleep:idle:handle-lid-switch nixos-rebuild switch;
          popd &> /dev/null;
          exit 0;
        elif [ "$1" = "update" ]; then
          pushd ${config.systemSettings.dotfilesDir} &> /dev/null;
          nix flake update "''${@:2}";
          popd &> /dev/null;
          #if [ "$#" -eq 1 ]; then
          #  pushd ${config.systemSettings.secretsFlakeDir} &> /dev/null;
          #  nix flake update;
          #  popd &> /dev/null;
          #fi
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
        elif [ "$1" = "build" ]; then
          chown -R 0:0 ${config.systemSettings.dotfilesDir};
          chown -R 0:0 ${config.systemSettings.secretsFlakeDir};
          pushd ${config.systemSettings.dotfilesDir} &> /dev/null;
          if [ "$#" -gt 1 ]; then
            hoststobuild=("''${@:2}")
            for i in "''${hoststobuild[@]}"
            do
              systemd-inhibit --what sleep:idle:handle-lid-switch nixos-rebuild build --flake .#$i;
              systemd-inhibit --what sleep:idle:handle-lid-switch attic push emmet ./result;
              systemd-inhibit --what sleep:idle:handle-lid-switch rm ./result;
            done
          else
            hoststobuild=($(find ${config.systemSettings.dotfilesDir}/hosts -maxdepth 1 -type d \! \( -name TEMPLATE \) \! \( -name hosts \) -exec basename {} \; | xargs -d " "))
            for i in "''${hoststobuild[@]}"
            do
              systemd-inhibit --what sleep:idle:handle-lid-switch nixos-rebuild build --flake .#$i;
              systemd-inhibit --what sleep:idle:handle-lid-switch attic push emmet ./result;
              systemd-inhibit --what sleep:idle:handle-lid-switch rm ./result;
            done
          exit 0;
          fi
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
            systemd-inhibit --what sleep:idle:handle-lid-switch nix-collect-garbage --delete-old;
          elif [ "$2" ]; then
            systemd-inhibit --what sleep:idle:handle-lid-switch nix-collect-garbage --delete-older-than $2;
          else
            systemd-inhibit --what sleep:idle:handle-lid-switch nix-collect-garbage --delete-older-than 30d;
          fi
          exit 0;
        fi
      '')
    ];

    # FIXME this thing doesn't work at all
    systemd.services."phoenix-system-builder" = lib.mkIf config.systemSettings.systemBuilder.enable {
      path = with pkgs; [
        pkgs-stable.openssh
        git
        nix
        nixos-rebuild
      ];
      script = ''
                set -euo pipefail
        	export NIX_PATH="nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos:nixos-config=/etc/nixos/configuration.nix"
                echo "navigating to /etc/nixos";
                cd ${config.systemSettings.dotfilesDir};
                echo "running git pull";
                ${pkgs.git}/bin/git pull;
                echo "running nix flake update";
                nix flake update;
                ${pkgs.git}/bin/git stage *;
                ${pkgs.git}/bin/git commit -m "Updated system" || true;
                ${pkgs.git}/bin/git push || true;
                cd ${config.systemSettings.secretsFlakeDir};
                ${pkgs.git}/bin/git pull;
                chown -R 0:0 ${config.systemSettings.dotfilesDir};
                chown -R 0:0 ${config.systemSettings.secretsFlakeDir};
                cd ${config.systemSettings.dotfilesDir};
                ${config.system.build.nixos-rebuild}/bin/nixos-rebuild build --flake .#snowfire;
                ${pkgs.attic-client}/bin/attic push emmet ./result;
                rm ./result;
                ${config.system.build.nixos-rebuild}/bin/nixos-rebuild build --flake .#duskfall;
                ${pkgs.attic-client}/bin/attic push emmet ./result;
                rm ./result;
                ${config.system.build.nixos-rebuild}/bin/nixos-rebuild build --flake .#zenith;
                ${pkgs.attic-client}/bin/attic push emmet ./result;
                rm ./result;
                ${config.system.build.nixos-rebuild}/bin/nixos-rebuild build --flake .#stardust;
                ${pkgs.attic-client}/bin/attic push emmet ./result;
                rm ./result;
                ${config.system.build.nixos-rebuild}/bin/nixos-rebuild build --flake .#ori;
                ${pkgs.attic-client}/bin/attic push emmet ./result;
                rm ./result;
      '';
      serviceConfig = {
        Type = "simple";
        User = "root";
      };
    };
    systemd.timers."phoenix-system-builder-auto" = lib.mkIf config.systemSettings.systemBuilder.enable {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = config.systemSettings.systemBuilder.buildCronExpression;
        Unit = "phoenix-system-builder.service";
      };
    };
  };
}
