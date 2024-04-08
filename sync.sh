#!/bin/sh

# Script to synchronize system state
# with configuration files for nixos system
# and home-manager

# Rebuild system
sudo nixos-rebuild switch --flake ~/.dotfiles#system;

# Install and build home-manager configuration
home-manager switch --flake ~/.dotfiles#user;

~/.dotfiles/sync-posthook.sh
