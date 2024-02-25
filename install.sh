#!/bin/sh

# Automated script to install my dotfiles

nix-shell -p git --command "git clone https://gitlab.com/librephoenix/nixos-config ~/.dotfiles"
sudo nixos-generate-config --show-hardware-config > ~/.dotfiles/system/hardware-configuration.nix
sed -i "0,/emmet/s//$(whoami)/" flake.nix
sed -i "0,/Emmet/s//$(getent passwd $(whoami) | cut -d ':' -f 5 | cut -d ',' -f 1)/" flake.nix
if [ -z "$EDITOR" ]; then
    EDITOR=nano;
fi
$EDITOR ~/.dotfiles/flake.nix;
sudo nixos-rebuild switch --flake ~/.dotfiles#system;
nix run home-manager/master --extra-experimental-features nix-command --extra-experimental-features flakes -- switch --flake ~/.dotfiles#user;
sudo ~/.dotfiles/harden.sh;
