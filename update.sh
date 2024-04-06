#!/bin/sh

# Automated script to update my non-primary systems
# to be in sync with upstream git repo while
# preserving local edits to dotfiles via git stash

# Relax permissions temporarily so git can work
sudo ~/.dotfiles/soften.sh ~/.dotfiles;

# Stash local edits, pull changes, and re-apply local edits
git stash
git pull
git stash apply

# Permissions for files that should be owned by root
sudo ~/.dotfiles/harden.sh ~/.dotfiles;

# Rebuild system
sudo nixos-rebuild switch --flake ~/.dotfiles#system;

# Install and build home-manager configuration
home-manager --extra-experimental-features nix-command --extra-experimental-features flakes -- switch --flake ~/.dotfiles#user;

