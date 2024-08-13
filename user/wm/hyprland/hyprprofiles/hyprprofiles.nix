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
      newprofile="$1"
      if [ $# -eq 1 ]; then
        if [ $newprofile = "Default" ]; then
          echo "" > ~/.hyprprofile;
        else
          echo $newprofile > ~/.hyprprofile;
        fi
        if [ -f ~/.config/hyprprofiles/$prevprofile/exit-hook.sh ]; then
          ~/.config/hyprprofiles/$prevprofile/exit-hook.sh;
        fi
        if [ -f ~/.config/hyprprofiles/$newprofile/start-hook.sh ]; then
          ~/.config/hyprprofiles/$newprofile/start-hook.sh;
        fi
      fi
    '')
    (pkgs.writeScriptBin "qutebrowser-hyprprofile" ''
      #!/bin/sh
      profile="$(cat ~/.hyprprofile)"
      if [[ $profile ]]; then
        container-open $profile $1;
      else
        qutebrowser --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag num-raster-threads=4 $1;
      fi
    '')
    (pkgs.makeDesktopItem {
      name = "qutebrowser-hyprprofile";
      desktopName = "Qutebrowser Hyprprofile";
      exec = "qutebrowser-hyprprofile %u";
      categories = ["Network" "WebBrowser"];
      keywords = ["Browser"];
      terminal = false;
      type = "Application";
      noDisplay = false;
      icon = "qutebrowser";
    })
    (pkgs.writeScriptBin "hyprprofile-dmenu" dmenuScript)
  ];
    xdg.mimeApps.defaultApplications = lib.mkForce {
    "text/html" = "qutebrowser-hyprprofile.desktop";
    "x-scheme-handler/http" = "qutebrowser-hyprprofile.desktop";
    "x-scheme-handler/https" = "qutebrowser-hyprprofile.desktop";
    "x-scheme-handler/about" = "qutebrowser-hyprprofile.desktop";
    "x-scheme-handler/unknown" = "qutebrowser-hyprprofile.desktop";
    };
  home.file.".config/hyprprofiles/" = {
    source = ./profiles;
    recursive = true;
    executable = true;
  };
}
