{ config, lib, pkgs, ... }:

{
  # Collection of useful CLI apps
  home.packages = with pkgs; [
    # Command Line
    neofetch lolcat cowsay
    cava
    gnugrep gnused
    killall
    libnotify
    bat exa fd bottom ripgrep
    rsync
    tmux
    htop
    hwinfo
    unzip
    octave
    brightnessctl
    w3m
    fzf
    pandoc
    pciutils
    (pkgs.callPackage ../pkgs/ytsub.nix { })
    (pkgs.callPackage ../pkgs/russ.nix { })
    (pkgs.callPackage ../pkgs/pokemon-colorscripts.nix { })
  ];

  imports = [
    ../bin/phoenix.nix # My nix command wrapper
    ../bin/ytsub-wrappers.nix # My ytsub wrapper
  ];
}
