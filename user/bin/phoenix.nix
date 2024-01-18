{ pkgs, userSettings, ... }:

let
  # This sets up my "phoenix" script with my configuration paths
  # =phoenix= is just my wrapper script for easier access to nix/nixos commands
  myPhoenixScript = ''
      RED='\033[0;31m'
      GREEN='\033[0;32m'
      ORANGE='\033[0;33m'
      BLUE='\033[0;34m'
      PURPLE='\033[0;35m'
      CYAN='\033[0;36m'
      NC='\033[0m'
      FRAMES="/ | \\ -"
      function non_blocking_wait {
        PID=$1
        if [ ! -d "/proc/$PID" ]; then
          wait $PID
          CODE=$?
        else
          CODE=127
        fi
      return $CODE
      }
      function animate_msg {
        pid=$!;
        while ps -p $pid > /dev/null;
        do
          for frame in $FRAMES;
          do
            printf "\r$frame $1";
          sleep 0.2;
          done
          non_blocking_wait $pid;
          status=$?;
        done
        if [ $status = 0 ]; then
          printf "\r$GREEN✓$NC $1$GREEN [Success!]$NC";
        else
          printf "\r$RED×$NC $1$RED [Failed!]$NC";
        fi
        printf "\n"
      }
      function sync_system {
        echo -e "$ORANGE### Syncing system configuration ###$NC"
        pushd ''+userSettings.dotfilesDir+'' &> /dev/null;
        if [ "$1" = "verbose" ]; then
          echo "Syncing system configuration (stack traces will be shown):"
          sudo systemd-run --no-ask-password --uid=0 --system --scope -p MemoryLimit=16000M -p CPUQuota=60% nixos-rebuild switch --flake .#system --show-trace;
        else
          sudo bash -c '
                  RED="\033[0;31m";
                  GREEN="\033[0;32m";
                  NC="\033[0m"
                  FRAMES="/ | \\ -";
                  systemd-run --no-ask-password --uid=0 --system --scope -p MemoryLimit=16000M -p CPUQuota=60% nixos-rebuild switch --flake .#system &> /dev/null &
                  pid=$!;
                  while ps -p $pid > /dev/null;
                  do
                    for frame in $FRAMES;
                    do
                      printf "\r$frame Syncing system configuration...";
                      sleep 0.2;
                    done
                    if [ ! -d "/proc/$pid" ]; then
                      wait $pid
                      status=$?
                    else
                      status=127
                    fi
                  done
                  if [ $status = 0 ]; then
                    printf "\r$GREEN✓$NC Syncing system configuration...$GREEN [Success!]$NC";
                  else
                    printf "\r$RED×$NC Syncing system configuration...$RED [Failed!]$NC";
                  fi
                  printf "\n"'
        fi
        popd &> /dev/null;
        echo -e "$ORANGE### System configuration sync finished ###$NC"
      }
      function sync_user {
        echo -e "$BLUE### Syncing user configuration ###$NC"
        pushd ''+userSettings.dotfilesDir+'' &> /dev/null;
        if [ "$1" = "verbose" ]; then
          echo "Syncing user configuration (stack traces will be shown):"
          echo "Running home-manager switch --flake .#user --show-trace"
          systemd-run --no-ask-password --uid=1000 --user --scope -p MemoryLimit=16000M -p CPUQuota=60% home-manager switch --flake .#user --show-trace;
          which xmobar &> /dev/null && echo "Killing old xmobar instances" && echo "Running killall xmobar" && killall xmobar &> /dev/null;
          which xmonad &> /dev/null && echo "Recompiling xmonad" && echo "Running xmonad --recompile && xmonad --restart" && xmonad --recompile && xmonad --restart;
          which emacsclient &> /dev/null && echo "Reloading emacs stylix theme" && echo "Running emacsclient --no-wait --eval \"(load-theme 'doom-stylix t nil)\"" && emacsclient --no-wait --eval "(load-theme 'doom-stylix t nil)";
          [ -f ~/.fehbg-stylix ] &> /dev/null && echo "Reapplying background from stylix via feh" && echo "Running ~/.fehbg-stylix" && ~/.fehbg-stylix;
          [ -f ~/.swaybg-stylix ] &> /dev/null && echo "Reapplying background from stylix via swaybg" && echo "Running ~/.swaybg-stylix" && ~/.swaybg-stylix;
        else
          systemd-run --no-ask-password --uid=1000 --user --scope -p MemoryLimit=16000M -p CPUQuota=60% home-manager switch --flake .#user &> /dev/null &
          animate_msg "Syncing user configuration..."
          which xmobar &> /dev/null && killall xmobar &> /dev/null &
          which xmonad &> /dev/null && xmonad --recompile &> /dev/null &
          animate_msg "Refreshing xmonad..."
          which emacsclient &> /dev/null && emacsclient --no-wait --eval "(load-theme 'doom-stylix t nil)" &> /dev/null &
          animate_msg "Reloading stylix theme..."
          [ -f ~/.fehbg-stylix ] &> /dev/null && ~/.fehbg-stylix &> /dev/null &
          [ -f ~/.swaybg-stylix ] &> /dev/null && ~/.swaybg-stylix &> /dev/null &
        fi
        popd &> /dev/null;
        echo -e "$BLUE### User configuration sync finished ###$NC"
      }
      function update_flake {
        echo -e "$CYAN### Updating flake and other package managers$NC";
        pushd ''+userSettings.dotfilesDir+'' &> /dev/null;
        if [ "$1" = "verbose" ]; then
          echo "Updating flake inputs";
          echo "Running nix flake update";
          nix flake update;
          if [ -d ~/.emacs.d/eaf/app/browser ] &> /dev/null;
          then
            echo "Updating npm dependencies for eaf"
            echo "Navigating to ~/.emacs.d/eaf/app/browser"
            pushd ~/.emacs.d/eaf/app/browser;
            echo "Running rm package*.json";
            rm package*.json;
            echo "Running npm install darkreader @mozilla/readability";
            npm install darkreader @mozilla/readability;
            echo "Running rm package*.json";
            rm package*.json;
            echo "Returning to ''+userSettings.dotfilesDir+''"
            popd &> /dev/null;
          fi
          echo ""
          echo "Updating flatpaks";
          echo "Running sudo flatpak update -y";
          sudo flatpak update -y;
        else
          nix flake update &> /dev/null &
          animate_msg "Updating flake inputs..."
          if [ -d ~/.emacs.d/eaf/app/browser ] &> /dev/null;
          then
            pushd ~/.emacs.d/eaf/app/browser &> /dev/null;
            rm package*.json &> /dev/null;
            npm install darkreader @mozilla/readability &> /dev/null &
            animate_msg "Updating npm dependencies for eaf...";
            rm package*.json &> /dev/null;
            popd;
          fi
          echo "Reqesting authentication for flatpak update.."
          sudo bash -c '
                  RED="\033[0;31m";
                  GREEN="\033[0;32m";
                  NC="\033[0m"
                  FRAMES="/ | \\ -";
                  flatpak update -y &> /dev/null &
                  pid=$!;
                  while ps -p $pid > /dev/null;
                  do
                    for frame in $FRAMES;
                    do
                      printf "\r$frame Updating flatpaks...";
                      sleep 0.2;
                    done
                    if [ ! -d "/proc/$pid" ]; then
                      wait $pid
                      status=$?
                    else
                      status=127
                    fi
                  done
                  if [ $status = 0 ]; then
                    printf "\r$GREEN✓$NC Updating flatpaks...$GREEN [Success!]$NC";
                  else
                    printf "\r$RED×$NC Updating flatpaks...$RED [Failed!]$NC";
                  fi
                  printf "\n"'
        fi
        popd &> /dev/null;
        echo -e "$CYAN### Flake and other updates finished ###$NC";
        echo -e "Please run$GREEN git$NC diff HEAD flake.lock inside ''+userSettings.dotfilesDir+(" "+'' to see flake input changes";'')+
      ''}
      if [ "$1" = "sync" ]; then
        if [ "$#" = 1 ]; then
          sync_system;
          sync_user;
        elif [ "$2" = "user" ]; then
          sync_user;
        elif [ "$2" = "system" ]; then
          sync_system;
        else
          echo "Please pass 'system' or 'user' if supplying a second argument"
        fi
      elif [ "$1" = "update" ]; then
        if [ "$#" -gt 1 ]; then
          echo "Warning: The 'update' command has no subcommands (no $2 subcommand)"
        fi
        update_flake;
      elif [ "$1" = "gc" ]; then
        if [ "$2" = "full" ]; then
          sudo nix-collect-garbage --delete-old;
          nix-collect-garbage --delete-old;
        elif [ "$2" ]; then
          sudo nix-collect-garbage --delete-older-than $2;
          nix-collect-garbage --delete-older-than $2;
        else
          sudo nix-collect-garbage --delete-older-than 30d;
          nix-collect-garbage --delete-older-than 30d;
        fi
      fi
    '';
in
{
  home.packages = [
    (pkgs.writeScriptBin "phoenix" myPhoenixScript)
  ];
}
