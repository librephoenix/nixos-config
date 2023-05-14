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
              ./app/git/git.nix # My git config
              ./app/games/games.nix # Various videogame apps
              ./style/stylix.nix # Styling and themes for my apps
              ./lang/cc/cc.nix # C and C++ tools
              ./lang/python/python.nix # Python
              ./lang/python/python-packages.nix # Extra Python packages I want
              ./lang/haskell/haskell.nix # Haskell tools
              ./lang/android/android.nix # Android developement
              ./lang/godot/godot.nix # Game development
            ];

  home.stateVersion = "22.11"; # Please read the comment before changing.

  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    
    # Core
    hello
    zsh
    alacritty
    kitty
    librewolf-wayland
    brave
    dmenu
    rofi
    feh
    git
    xmobar

    # Office
    libreoffice-qt
    mate.atril
    xournalpp
    gnome.geary
    autokey
    protonmail-bridge

    # File Managers
    ranger
    libsForQt5.dolphin
    libsForQt5.dolphin-plugins
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
    # TODO need ytsub somehow (sarowish/ytsub)
    audio-recorder

    # Command Line
    neofetch lolcat cowsay
    gnugrep gnused
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
    nodePackages.mermaid-cli
    
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
    twmn

    # Wayland Utils
    xdg-desktop-portal-wlr
    wtype
    # wl-clipboard-x11
    xorg.xlsclients
    glfw-wayland
    swayidle
    swaylock
    wlsunset
    wayshot
    wev
    
    # TODO Configure pipewire audio server

    # Virtual Machines
    libvirt
    virt-manager
    qemu_full
    lxc
    swtpm

    # Security
    keepassxc
    gnome.seahorse
    protonvpn-gui

    # Filesystems
    dosfstools
  ];

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./app/doom-emacs;
  };

  nixpkgs.overlays = [
    (self: super:
      {
        picom = super.picom.overrideAttrs (oldAttrs: rec {
        version = "unstable-2021-10-23";
        src = super.fetchFromGitHub {
          owner = "pijulius";
          repo = "picom";
          rev = "982bb43e5d4116f1a37a0bde01c9bda0b88705b9";
          sha256 = "sha256-YiuLScDV9UfgI1MiYRtjgRkJ0VuA1TExATA2nJSJMhM=";
        };

        meta = with builtins.lib; {
          description = "A fork of picom featuring better animations";
          homepage = "https://github.com/pijulius/picom";
        };
        });
      }
    )
  ];

  home.file.".librewolf/librewolf.overrides.cfg".text = ''
    pref("font.name.serif.x-western","Inconsolata");
    pref("font.size.variable.x-western",20);
    pref("browser.toolbars.bookmarks.visibility","always");
    pref("privacy.resisttFingerprinting.letterboxing", true);
    pref("network.http.referer.XOriginPolicy",2);
    pref("privacy.clearOnShutdown.history",false);
    pref("privacy.clearOnShutdown.downloads",false);
    pref("privacy.clearOnShutdown.cookies",false);
    pref("gfx.webrender.software.opengl",true);
    pref("webgl"webgl.disabled",false);
    '';

  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

}
