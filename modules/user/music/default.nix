{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.userSettings.music;
in
{
  options = {
    userSettings.music = {
      enable = lib.mkEnableOption "Enable apps for making music";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      rosegarden
      mediainfo
      easytag
      bottles
      # The following requires 64-bit FL Studio (FL64) to be installed to a bottle
      # With a bottle name of "FL Studio"
      (pkgs.writeShellScriptBin "flstudio" ''
        #!/bin/sh
        if [ -z "$1" ]
          then
            bottles-cli run -b "FL Studio" -p FL64
            #flatpak run --command=bottles-cli com.usebottles.bottles run -b FL\ Studio -p FL64
          else
            filepath=$(winepath --windows "$1")
            echo \'"$filepath"\'
            bottles-cli run -b "FL Studio" -p "FL64" --args \'"$filepath"\'
            #flatpak run --command=bottles-cli com.usebottles.bottles run -b FL\ Studio -p FL64 -args "$filepath"
          fi
      '')
      (pkgs.makeDesktopItem {
        name = "flstudio";
        desktopName = "FL Studio 64";
        exec = "flstudio %U";
        terminal = false;
        type = "Application";
        icon = "flstudio";
        mimeTypes = [ "application/octet-stream" ];
      })
      (stdenv.mkDerivation {
        name = "flstudio-icon";
        # icon from https://www.reddit.com/r/MacOS/comments/jtmp7z/i_made_icons_for_discord_spotify_and_fl_studio_in/
        src = [ ./flstudio.png ];

        unpackPhase = ''
          for srcFile in $src; do
            # Copy file into build dir
            cp $srcFile ./
          done
        '';

        installPhase = ''
          mkdir -p $out $out/share $out/share/pixmaps
          ls $src
          ls
          cp $src $out/share/pixmaps/flstudio.png
        '';
      })
    ];

    xdg.mimeApps.associations.added = {
      "application/octet-stream" = "flstudio.desktop;";
    };
  };
}
