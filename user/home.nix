{ config, lib, pkgs, myName, myEmail, myHomeDir, myDotfilesDir, myTheme, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = myName;
  home.homeDirectory = myHomeDir;

  programs.home-manager.enable = true;

  imports = [
              ./wm/xmonad/xmonad.nix # My xmonad config
              ./shell/sh.nix # My zsh and bash config
              ./bin/phoenix.nix # My nix command wrapper
              ./bin/ytsub-wrappers.nix # My ytsub wrapper
              ./app/doom-emacs/doom.nix # My doom emacs config
              ./app/terminal/alacritty.nix # My alacritty config
              ./app/terminal/kitty.nix # My kitty config
              ./app/git/git.nix # My git config
              ./app/games/games.nix # Various videogame apps
              ./style/stylix.nix # Styling and themes for my apps
              ./lang/cc/cc.nix # C and C++ tools
              #./lang/rust/rust.nix # Rust tools
              #./lang/python/python.nix # Python
              #./lang/python/python-packages.nix # Extra Python packages I want
              ./lang/haskell/haskell.nix # Haskell tools
              #./lang/android/android.nix # Android developement
              ./lang/godot/godot.nix # Game development
            ];

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = with pkgs; [
    # Core
    zsh
    alacritty
    kitty
    librewolf
    brave
    dmenu
    rofi
    keepmenu
    networkmanager_dmenu
    feh
    git
    syncthing
    flameshot

    # Office
    libreoffice-qt
    mate.atril
    xournalpp
    gnome.geary
    gnome.gnome-calendar
    newsflash
    #autokey
    protonmail-bridge

    # File Managers
    ranger
    xdragon

    # Media
    gimp-with-plugins
    krita
    cmus
    vlc
    mpv
    yt-dlp
    blender
    obs-studio
    libsForQt5.kdenlive
    movit
    mediainfo
    libmediainfo
    mediainfo-gui
    freetube
    audio-recorder
    pavucontrol
    gtkcord4

    # Command Line
    neofetch lolcat cowsay
    cava
    gnugrep gnused
    xorg.xkill
    killall
    libnotify
    bat exa fd bottom ripgrep
    rsync
    systeroid
    tmux
    htop
    hwinfo
    unzip
    octave
    brightnessctl
    w3m
    fzf
    hunspell hunspellDicts.en_US-large
    pandoc
    (pkgs.callPackage ./pkgs/ytsub.nix { })
    (pkgs.callPackage ./pkgs/pokemon-colorscripts.nix { })

    # Various dev packages
    texinfo
    libffi zlib
    nodePackages.ungit

    # Compositor and Desktop Utils
    picom
    alttab
    xorg.xcursorthemes

    # X Utils
    xdotool
    xclip
    ddcutil
    sct
    caffeine-ng

    # Wayland Utils
    # xdg-desktop-portal-wlr
    # wtype
    # wl-clipboard-x11
    # xorg.xlsclients
    # glfw-wayland
    # swayidle
    # swaylock
    # wlsunset
    # wayshot
    # wev
    
    # TODO Configure pipewire audio server

    # Virtual Machines and wine
    libvirt
    virt-manager
    qemu_full
    lxc
    swtpm
    bottles

    # Security
    keepassxc
    gnome.seahorse
    protonvpn-gui

    # Filesystems
    dosfstools

    # Extra packages
    flatpak
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

  home.file.".librewolf/librewolf.overrides.cfg".text = ''
    defaultPref("font.name.serif.x-western","Inconsolata");
    defaultPref("font.size.variable.x-western",20);
    defaultPref("browser.toolbars.bookmarks.visibility","always");
    defaultPref("privacy.resisttFingerprinting.letterboxing", true);
    defaultPref("network.http.referer.XOriginPolicy",2);
    defaultPref("privacy.clearOnShutdown.history",false);
    defaultPref("privacy.clearOnShutdown.downloads",false);
    defaultPref("privacy.clearOnShutdown.cookies",false);
    defaultPref("gfx.webrender.software.opengl",true);
    defaultPref("webgl.disabled",false);
    pref("font.name.serif.x-western","Inconsolata");
    pref("font.size.variable.x-western",20);
    pref("browser.toolbars.bookmarks.visibility","always");
    pref("privacy.resisttFingerprinting.letterboxing", true);
    pref("network.http.referer.XOriginPolicy",2);
    pref("privacy.clearOnShutdown.history",false);
    pref("privacy.clearOnShutdown.downloads",false);
    pref("privacy.clearOnShutdown.cookies",false);
    pref("gfx.webrender.software.opengl",true);
    pref("webgl.disabled",false);
    '';

  home.file.".config/networkmanager-dmenu/config.ini".text = ''
    [dmenu]
    dmenu_command = rofi -show dmenu
    compact = True
    wifi_chars = ▂▄▆█
    list_saved = True

    [editor]
    terminal = alacritty
    # gui_if_available = <True or False> (Default: True)
  '';

  home.sessionVariables = {
    EDITOR = "emacsclient";
    XDG_DATA_DIRS = "$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share"; # lets flatpak work
  };

  # extra packages

}
