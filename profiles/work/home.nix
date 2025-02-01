{ config, pkgs, pkgs-stable, userSettings, ... }:

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
              ../../user/app/emacs # Emacs config that I'm currently rebuilding
              ../../user/app/ranger/ranger.nix # My ranger file manager config
              ../../user/app/git/git.nix # My git config
              ../../user/app/blender # My blender config
              ../../user/app/keepass/keepass.nix # My password manager
              (./. + "../../../user/app/browser"+("/"+userSettings.browser)+".nix") # My default browser selected from flake
              ../../user/app/virtualization/virtualization.nix # Virtual machines
              #../../user/app/flatpak/flatpak.nix # Flatpaks
              ../../user/style/stylix.nix # Styling and themes for my apps
              ../../user/hardware/bluetooth.nix # Bluetooth
  ];

  # TODO make nix path follow flakes

  nix.package = pkgs.nix;
  nix.settings = {
    substituters = [
      "https://cache.nixos.org"
      "https://hyprland.cachix.org"
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = (with pkgs; [
    # Core
    zsh
    alacritty
    brave
    qutebrowser
    git
    syncthing

    # Office
    nextcloud-client
    (pkgs-stable.libreoffice-fresh)
    mate.atril
    openboard
    xournalpp
    adwaita-icon-theme
    shared-mime-info
    glib
    newsflash
    foliate
    nautilus
    gnome-calendar
    seahorse
    gnome-maps
    openvpn
    protonmail-bridge
    texliveSmall
    numbat
    element-desktop

    openai-whisper-cpp

    wine64
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
    krita
    pinta
    inkscape
    godot_4
    (pkgs-stable.lollypop.override { youtubeSupport = false; })
    vlc
    mpv
    yt-dlp_git
    blender-hip
    freecad
    libresprite
    (pkgs.appimageTools.wrapType2 {
      name = "Cura";
      pname = "Cura";
      version = "5.9.0";
      src = fetchurl {
        url = "https://github.com/Ultimaker/Cura/releases/download/5.9.0/UltiMaker-Cura-5.9.0-linux-X64.AppImage";
        hash = "sha256-STtVeM4Zs+PVSRO3cI0LxnjRDhOxSlttZF+2RIXnAp4=";
      };
      extraPkgs = pkgs: with pkgs; [];
     })
    (pkgs.makeDesktopItem {
      name = "Cura";
      desktopName = "Cura";
      exec = "env QT_QPA_PLATFORM=xcb Cura %i";
      terminal = false;
      type = "Application";
     })
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
    cheese
    #ardour
    #rosegarden
    tenacity

    # Various dev packages
    remmina
    sshfs
    texinfo
    libffi zlib
    nodePackages.ungit
    ventoy
    kdenlive
  ]);

  services.syncthing.enable = true;
  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

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
      XDG_PROJECTS_DIR = "${config.home.homeDirectory}/Projects";
      XDG_CLOUD_DIR = "${config.home.homeDirectory}/Drive";
      XDG_BOOK_DIR = "${config.home.homeDirectory}/Media/Books";
      XDG_VM_DIR = "${config.home.homeDirectory}/Machines";
      XDG_NOTES_DIR = "${config.home.homeDirectory}/Notes";
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

  services.pasystray.enable = true;
  services.pasystray.extraOptions = [ "--no-notify" ];

}
