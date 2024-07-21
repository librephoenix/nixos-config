{ config, pkgs, pkgs-stable, pkgs-kdenlive, userSettings, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = userSettings.username;
  home.homeDirectory = "/home/"+userSettings.username;

  programs.home-manager.enable = true;

  imports = [
              (./. + "../../../user/wm"+("/"+userSettings.wm+"/"+userSettings.wm)+".nix") # My window manager selected from flake
              ../../user/shell/sh.nix # My zsh and bash config
              ../../user/shell/cli-collection.nix # Useful CLI apps
              ../../user/app/doom-emacs/doom.nix # My doom emacs config
              ../../user/app/ranger/ranger.nix # My ranger file manager config
              ../../user/app/git/git.nix # My git config
              ../../user/app/keepass/keepass.nix # My password manager
              (./. + "../../../user/app/browser"+("/"+userSettings.browser)+".nix") # My default browser selected from flake
              ../../user/app/virtualization/virtualization.nix # Virtual machines
              #../../user/app/flatpak/flatpak.nix # Flatpaks
              ../../user/style/stylix.nix # Styling and themes for my apps
              ../../user/lang/cc/cc.nix # C and C++ tools
              ../../user/lang/godot/godot.nix # Game development
              #../../user/pkgs/blockbench.nix # Blockbench ## marked as insecure
              ../../user/hardware/bluetooth.nix # Bluetooth
            ];

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = (with pkgs; [
    # Core
    zsh
    alacritty
    librewolf
    brave
    qutebrowser
    git
    syncthing

    # Office
    libreoffice-fresh
    mate.atril
    openboard
    xournalpp
    gnome.adwaita-icon-theme
    shared-mime-info
    glib
    newsflash
    foliate
    gnome.nautilus
    gnome.gnome-calendar
    gnome.seahorse
    gnome.gnome-maps
    openvpn
    protonmail-bridge
    texliveSmall
    numbat
    element-desktop-wayland

    openai-whisper-cpp

    wine
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
      mimeTypes = ["application/octet-stream"];
    })
    (stdenv.mkDerivation {
      name = "flstudio-icon";
      # icon from https://www.reddit.com/r/MacOS/comments/jtmp7z/i_made_icons_for_discord_spotify_and_fl_studio_in/
      src = [ ../../user/pkgs/flstudio.png ];

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

    # Media
    gimp
    pinta
    krita
    inkscape
    (pkgs-stable.lollypop.override { youtubeSupport = false; })
    vlc
    mpv
    yt-dlp
    blender-hip
    # cura is moderately broken on wayland, so use xwayland
    (pkgs-stable.cura.overrideAttrs (oldAttrs: {
      postInstall = oldAttrs.postInstall + ''cp -rf ${(pkgs.makeDesktopItem {
          name = "com.ultimaker.cura";
          icon = "cura-icon";
          desktopName = "Cura";
          exec = "env QT_QPA_PLATFORM=xcb ${pkgs-stable.cura}/bin/cura %F";
          tryExec = "env QT_QPA_PLATFORM=xcb ${pkgs-stable.cura}/bin/cura";
          terminal = false;
          type = "Application";
          categories = ["Graphics"];
          mimeTypes = ["model/stl" "application/vnd.ms-3mfdocument" "application/prs.wavefront-obj"
                       "image/bmp" "image/gif" "image/jpeg" "image/png" "text/x-gcode" "application/x-amf"
                       "application/x-ply" "application/x-ctm" "model/vnd.collada+xml" "model/gltf-binary"
                       "model/gltf+json" "model/vnd.collada+xml+zip"];
          })}/share/applications $out/share'';
    }))
    (pkgs.writeShellScriptBin "curax" ''env QT_QPA_PLATFORM=xcb ${pkgs-stable.cura}/bin/cura'')
    (pkgs-stable.curaengine_stable)
    openscad
    (stdenv.mkDerivation {
      name = "cura-slicer";
      version = "0.0.7";
      src = fetchFromGitHub {
        owner = "Spiritdude";
        repo = "Cura-CLI-Wrapper";
        rev = "ff076db33cfefb770e1824461a6336288f9459c7";
        sha256 = "sha256-BkvdlqUqoTYEJpCCT3Utq+ZBU7g45JZFJjGhFEXPXi4=";
      };
      phases = "installPhase";
      installPhase = ''
        mkdir -p $out $out/bin $out/share $out/share/cura-slicer
        cp $src/cura-slicer $out/bin
        cp $src/settings/fdmprinter.def.json $out/share/cura-slicer
        cp $src/settings/base.ini $out/share/cura-slicer
        sed -i 's+#!/usr/bin/perl+#! /usr/bin/env nix-shell\n#! nix-shell -i perl -p perl538 perl538Packages.JSON+g' $out/bin/cura-slicer
        sed -i 's+/usr/share+/home/${userSettings.username}/.nix-profile/share+g' $out/bin/cura-slicer
      '';
      propagatedBuildInputs = with pkgs-stable; [
        curaengine_stable
      ];
    })
    obs-studio
    ffmpeg
    (pkgs.writeScriptBin "kdenlive-accel" ''
      #!/bin/sh
      DRI_PRIME=0 kdenlive "$1"
    '')
    movit
    mediainfo
    libmediainfo
    audio-recorder
    gnome.cheese
    ardour
    rosegarden
    tenacity

    # Various dev packages
    texinfo
    libffi zlib
    nodePackages.ungit
    ventoy
    nextcloud-client
  ]) ++ ([ pkgs-kdenlive.kdenlive ]);

  home.file.".local/share/pixmaps/nixos-snowflake-stylix.svg".source =
    config.lib.stylix.colors {
      template = builtins.readFile ../../user/pkgs/nixos-snowflake-stylix.svg.mustache;
      extension = "svg";
    };

  services.syncthing.enable = true;

  xdg.enable = true;
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    music = "${config.home.homeDirectory}/Media/Music";
    videos = "${config.home.homeDirectory}/Media/Videos";
    pictures = "${config.home.homeDirectory}/Media/Pictures";
    templates = "${config.home.homeDirectory}/Templates";
    download = "${config.home.homeDirectory}/Downloads";
    documents = "${config.home.homeDirectory}/Documents";
    desktop = null;
    publicShare = null;
    extraConfig = {
      XDG_DOTFILES_DIR = "${config.home.homeDirectory}/.dotfiles";
      XDG_ARCHIVE_DIR = "${config.home.homeDirectory}/Archive";
      XDG_VM_DIR = "${config.home.homeDirectory}/Machines";
      XDG_ORG_DIR = "${config.home.homeDirectory}/Org";
      XDG_PODCAST_DIR = "${config.home.homeDirectory}/Media/Podcasts";
      XDG_BOOK_DIR = "${config.home.homeDirectory}/Media/Books";
    };
  };
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;
  xdg.mimeApps.associations.added = {
    # TODO fix mime associations, most of them are totally broken :(
    "application/octet-stream" = "flstudio.desktop;";
  };

  home.sessionVariables = {
    EDITOR = userSettings.editor;
    SPAWNEDITOR = userSettings.spawnEditor;
    TERM = userSettings.term;
    BROWSER = userSettings.browser;
  };

  news.display = "silent";

  gtk.iconTheme = {
    package = pkgs.papirus-icon-theme;
    name = if (config.stylix.polarity == "dark") then "Papirus-Dark" else "Papirus-Light";
  };

}
