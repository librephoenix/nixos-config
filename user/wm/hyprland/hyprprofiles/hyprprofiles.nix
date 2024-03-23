{ config, lib, pkgs, dmenuCmd ? "rofi -dmenu", ... }:
let
  dmenuScript = ''
      #!/bin/sh
      choice="$(\ls ~/.config/hyprprofiles | ''+dmenuCmd+'')";
      hyprprofile $choice;
  '';
in
{
  home.packages = [
    (pkgs.writeScriptBin "hyprprofile" ''
      #!/bin/sh
      prevprofile="$(cat ~/.hyprprofile)"
      newprofile=$1
      if [ $# -eq 1 ]; then
        if [ $newprofile = "Default" ]; then
          echo "" > ~/.hyprprofile;
        else
          echo $newprofile > ~/.hyprprofile;
        fi
        if [ -f ~/.config/hyprprofiles/$prevprofile/exit-hook.sh ]; then
          exec ~/.config/hyprprofiles/$prevprofile/exit-hook.sh;
        fi
        if [ -f ~/.config/hyprprofiles/$newprofile/start-hook.sh ]; then
          exec ~/.config/hyprprofiles/$newprofile/start-hook.sh;
        fi
      fi
    '')
    (pkgs.writeScriptBin "hyprprofile-dmenu" dmenuScript)
  ];
  home.file.".config/hyprprofiles/" = {
    source = ./profiles;
    recursive = true;
    executable = true;
  };
}
