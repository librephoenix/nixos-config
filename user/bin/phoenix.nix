{ config, lib, pkgs, username, dotfilesDir, ... }:

let
  # This sets up my "phoenix" script with my configuration paths
  # =phoenix= is just my wrapper script for easier access to nix/nixos commands
  myPhoenixScript = ''
      if [ "$1" = "sync" ]; then
        if [ "$2" != "user" ]; then
          pushd ''+dotfilesDir+'';
          sudo nixos-rebuild switch --flake .#;
          popd;
        elif [ "2" != "system" ]; then
          pushd ''+dotfilesDir+'';
          home-manager switch --flake .#''+username+'';
          popd;
          which xmobar &> /dev/null && killall xmobar;
          which xmonad &> /dev/null && xmonad --recompile && xmonad --restart;
          which emacsclient &> /dev/null && emacsclient --no-wait --eval "(load-theme 'doom-stylix t nil)";
          [ -f ~/.fehbg-stylix ] &> /dev/null && ~/.fehbg-stylix;
        fi
      elif [ "$1" = "update" ]; then
        pushd ''+dotfilesDir+'';
        nix flake update;
        popd;
        if [ -d ~/.emacs.d/eaf/app/browser ]
        then
          pushd ~/.emacs.d/eaf/app/browser;
          rm package*.json;
          npm install darkreader @mozilla/readability && rm package*.json;
          popd;
        fi
      elif [ "$1" = "gc" ]; then
        if [ "$2" = "full" ]; then
          sudo nix-collect-garbage --delete-old;
        elif [ "$2" ]; then
          sudo nix-collect-garbage --delete-older-than $2;
        else
          sudo nix-collect-garbage --delete-older-than 30d;
        fi
      fi
    '';
in
{
  home.packages = [
    (pkgs.writeScriptBin "phoenix" myPhoenixScript)
  ];
}
