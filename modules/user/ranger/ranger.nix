{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.ranger;
in {
  options = {
    userSettings.ranger = {
      enable = lib.mkEnableOption "Enable ranger file manager";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      ranger
      ripdrag
      highlight
      poppler_utils
      librsvg
      ffmpegthumbnailer
      # TODO fix cbx script
      (pkgs.writeScriptBin "cbx" ''
        # this lets my copy and paste images and/or plaintext of files directly out of ranger
        if [ "$#" -le "2" ]; then
          if [ "$1" = "copy" -o "$1" = "cut" ]; then
            if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
              wl-copy < $2;
            else
              # xclip -selection clipboard -t $(file -b --mime-type $2) -i $2;
              xclip -selection clipboard -t image/png -i $2;
            fi
          fi
        fi
      '')
    ];

    xdg.mimeApps.associations.added = {
      "inode/directory" = "ranger.desktop";
    };
    home.file.".config/ranger/rc.conf".source = ./rc.conf;
    home.file.".config/ranger/rifle.conf".source = ./rifle.conf;
    home.file.".config/ranger/scope.sh" = {
      source = ./scope.sh;
      executable = true;
    };
    home.file.".config/ranger/commands.py" = {
      source = ./commands.py;
      executable = true;
    };
    home.file.".config/ranger/commands_full.py" = {
      source = ./commands_full.py;
      executable = true;
    };
    home.file.".config/ranger/colorschemes/hail.py" = {
      source = ./colorschemes/hail.py;
      executable = true;
    };
  };
}
