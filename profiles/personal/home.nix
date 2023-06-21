{ config, lib, pkgs, python3Packages, nix-doom-emacs, stylix, myName, myEmail, myHomeDir, myDotfilesDir, myTheme, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = myName;
  home.homeDirectory = myHomeDir;

  programs.home-manager.enable = true;

  imports = [
              nix-doom-emacs.hmModule
              stylix.homeManagerModules.stylix
              ../../user/wm/xmonad/xmonad.nix # My xmonad config
              ../../user/shell/sh.nix # My zsh and bash config
              ../../user/shell/cli-collection.nix # Useful CLI apps
              ../../user/bin/phoenix.nix # My nix command wrapper
              ../../user/app/doom-emacs/doom.nix # My doom emacs config
              ../../user/app/ranger/ranger.nix # My ranger file manager config
              ../../user/app/git/git.nix # My git config
              ../../user/app/keepass/keepass.nix # My password manager
              ../../user/app/browser/librewolf.nix # My default browser
              ../../user/app/games/games.nix # Various videogame apps
              ../../user/app/virtualization/virtualization.nix # Virtual machines
              ../../user/app/flatpak/flatpak.nix # Flatpaks
              ../../user/style/stylix.nix # Styling and themes for my apps
              ../../user/lang/cc/cc.nix # C and C++ tools
              ../../user/lang/godot/godot.nix # Game development
            ];

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = with pkgs; [
    # Core
    zsh
    alacritty
    librewolf
    brave
    dmenu
    rofi
    git
    syncthing

    # Office
    libreoffice-qt
    mate.atril
    xournalpp
    glib
    gnome.geary
    gnome.gnome-calendar
    gnome.seahorse
    gnome.gnome-maps
    newsflash
    #autokey
    protonmail-bridge
    openvpn

    # Media
    gimp-with-plugins
    krita
    musikcube
    vlc
    mpv
    yt-dlp
    freetube
    blender
    obs-studio
    libsForQt5.kdenlive
    movit
    mediainfo
    libmediainfo
    mediainfo-gui
    audio-recorder
    gtkcord4

    # Various dev packages
    texinfo
    libffi zlib
    nodePackages.ungit

  ];

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
      XDG_GAME_DIR = "${config.home.homeDirectory}/Media/Games";
      XDG_GAME_SAVE_DIR = "${config.home.homeDirectory}/Media/Game Saves";
      XDG_PODCAST_DIR = "${config.home.homeDirectory}/Media/Podcasts";
      XDG_BOOK_DIR = "${config.home.homeDirectory}/Media/Books";
    };
  };
  xdg.mime.enable = true;
  xdg.mimeApps.enable = true;

}
