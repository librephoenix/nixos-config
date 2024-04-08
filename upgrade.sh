#!/bin/sh

# Script to update system and sync
# Does not pull changes from git

# Update flake
~/.dotfiles/update.sh;

# Synchronize system
~/.dotfiles/sync.sh;
