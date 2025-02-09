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
    nixpkgs.overlays = [
      (self: super:
        {
          ranger = super.ranger.overrideAttrs (oldAttrs: rec {
          preConfigure = ''
            substituteInPlace ranger/__init__.py \
              --replace "DEFAULT_PAGER = 'less'" "DEFAULT_PAGER = '${lib.getBin pkgs.bat}/bin/bat'"
        
            # give image previews out of the box when building with w3m
            substituteInPlace ranger/config/rc.conf \
              --replace "set preview_images false" "set preview_images true"

            # adds this patch: https://github.com/ranger/ranger/pull/1758
            # fixes a bug for kitty users that use image previews
            substituteInPlace ranger/ext/img_display.py \
              --replace "self.image_id -= 1" "self.image_id = max(0, self.image_id - 1)"

            # fixes the .desktop file
            substituteInPlace doc/ranger.desktop \
              --replace "Icon=utilities-terminal" "Icon=user-desktop"
            substituteInPlace doc/ranger.desktop \
              --replace "Terminal=true" "Terminal=false"
            substituteInPlace doc/ranger.desktop \
              --replace "Exec=ranger" "Exec=kitty -e ranger %U"
          '';
          });
        }
      )
    ];

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
