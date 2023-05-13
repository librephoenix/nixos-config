{ config, lib, pkgs, myName, myEmail, myHomeDir, myDotfilesDir, ... }:

let
  # This sets up my "phoenix" script with my configuration paths
  # =phoenix= is just my wrapper script for easier access to nix/nixos commands
  myPhoenixScript = ''
      if [ "$1" = "sync" ]; then
        if [ "$2" != "user" ]; then
          pushd ''+myDotfilesDir+'';
          sudo nixos-rebuild switch --flake .#;
          popd;
        fi
        if [ "2" != "system" ]; then
          pushd ''+myDotfilesDir+'';
          home-manager switch --flake .#''+myName+'';
          popd;
        fi
      elif [ "$1" = "update" ]; then
        pushd ''+myDotfilesDir+'';
        nix flake update;
        popd;
      fi
    '';

in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = myName;
  home.homeDirectory = myHomeDir;

  programs.home-manager.enable = true;

  imports = [
              ./wm/xmonad/xmonad.nix # My xmonad config
              ./shell/sh.nix # My zsh and bash config
             ./app/git/git.nix # My git config
              ./style/stylix.nix # Styling and themes for my apps
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
    (pkgs.writeScriptBin "phoenix" myPhoenixScript)

    # Office
    libreoffice-qt
    mate.atril
    xournalpp
    gnome.geary
    autokey
    protonmail-bridge
    syncthingtray-minimal

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

    # Games
    #TODO need flatpak steam
    gamehub
    retroarch
    libretro.mgba
    libretro.desmume
    libretro.dolphin
    libretro.citra
    libretro.genesis-plus-gx
    airshipper
    qjoypad
    # TODO need flatpak minecraft

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
    
    # Development

      # Android
      android-tools
      android-udev-rules

      # CC
      gcc
      gnumake
      cmake
      autoconf
      automake
      libtool

      # Python
      python310Full
      imath
      pystring
      python310Packages.cffi
      python310Packages.dbus-python
      python310Packages.wheel
      python310Packages.pyyaml
      python310Packages.zipp
      python310Packages.xlib
      python310Packages.libvirt
      python310Packages.pybind11
      python310Packages.pyatspi
      python310Packages.attrs
      python310Packages.autocommand
      python310Packages.bcrypt
      python310Packages.pycairo
      python310Packages.certifi
      python310Packages.chardet
      python310Packages.click
      python310Packages.cryptography
      python310Packages.cssselect
      python310Packages.python-dateutil
      python310Packages.distro
      python310Packages.dnspython
      python310Packages.evdev
      python310Packages.ewmh
      python310Packages.fastjsonschema
      python310Packages.fido2
      python310Packages.python-gnupg
      python310Packages.pygobject3
      python310Packages.idna
      python310Packages.importlib-metadata
      python310Packages.inflect
      python310Packages.isodate
      python310Packages.jeepney
      python310Packages.keyring
      python310Packages.lxml
      python310Packages.markdown
      python310Packages.markupsafe
      python310Packages.more-itertools
      python310Packages.numpy
      python310Packages.ordered-set
      python310Packages.packaging
      python310Packages.pillow
      python310Packages.pip
      python310Packages.platformdirs
      python310Packages.ply
      python310Packages.prettytable
      python310Packages.proton-client
      python310Packages.protonvpn-nm-lib
      python310Packages.psutil
      python310Packages.pulsectl
      python310Packages.pycparser
      python310Packages.pycups
      python310Packages.pycurl
      python310Packages.pydantic
      python310Packages.pyinotify
      python310Packages.pyopenssl
      python310Packages.pyparsing
      python310Packages.pyqt5
      python310Packages.pyqt5_sip
      python310Packages.pyscard
      python310Packages.pythondialog
      python310Packages.pyxdg
      python310Packages.rdflib
      python310Packages.requests
      python310Packages.secretstorage
      python310Packages.setproctitle
      python310Packages.setuptools
      python310Packages.six
      python310Packages.systemd
      python310Packages.tomli
      python310Packages.urllib3
      python310Packages.wcwidth
      python310Packages.websockets
      python310Packages.python-zbar

      # Haskell
      haskellPackages.haskell-language-server
      haskellPackages.stack

      # Gamedev
      godot

      # Other
      texinfo
      libffi zlib
      nodePackages.ungit

    # Compositor and Desktop Utils
    picom
    alttab
    xwinwrap
    xorg.xcursorthemes

    # X Utils
    xdotool
    xclip
    ddcutil
    sct
    caffeine-ng
    twmn
    networkmanagerapplet

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

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/emmet/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

}
