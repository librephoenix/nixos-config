{ config, lib, pkgs, myName, myDotfilesDir, ... }:

let
  # This sets up my "phoenix" script with my configuration paths
  # =phoenix= is just my wrapper script for easier access to nix/nixos commands
  myPhoenixScript = ''
      if [ "$1" = "sync" ]; then
        if [ "$2" != "user" ]; then
          pushd ''+myDotfilesDir+'';
          sudo nixos-rebuild switch --flake .#;
          popd;
        fi
        if [ "2" != "system" ]; then
          pushd ''+myDotfilesDir+'';
          home-manager switch --flake .#''+myName+'';
          popd;
          killall xmobar;
          xmonad --recompile && xmonad --restart;
          emacsclient --no-wait --eval "(load-theme 'doom-stylix t nil)";
        fi
      elif [ "$1" = "update" ]; then
        pushd ''+myDotfilesDir+'';
        nix flake update;
        popd;
      fi
    '';
in
{
  home.packages = [
    (pkgs.writeScriptBin "phoenix" myPhoenixScript)
  ];
}
