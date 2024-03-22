{ pkgs, ... }:

{
  # Collection of useful CLI apps
  home.packages = with pkgs; [
    # Command Line
    disfetch lolcat cowsay
    starfetch
    cava
    killall
    libnotify
    timer
    brightnessctl
    gnugrep
    bat eza fd bottom ripgrep
    rsync
    unzip
    tmux
    w3m
    pandoc
    hwinfo
    pciutils
    numbat
    (pkgs.callPackage ../pkgs/smartcalc.nix { })
    #(pkgs.writeShellScriptBin "sc" ''smartcalc'')
    #(pkgs.callPackage ../pkgs/pokemon-colorscripts.nix { })
    #(pkgs.python3Packages.callPackage ../pkgs/impressive.nix { })
    (pkgs.writeShellScriptBin "airplane-mode" ''
      #!/bin/sh
      connectivity="$(nmcli n connectivity)"
      if [ "$connectivity" == "full" ]
      then
          nmcli n off
      else
          nmcli n on
      fi
    '')
    vim neovim
  ];

  imports = [
    ../bin/phoenix.nix # My nix command wrapper
  ];
}
