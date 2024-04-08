#!/bin/sh

# Automated script to update my non-primary systems
# to be in sync with upstream git repo while
# preserving local edits to dotfiles via git stash

# Relax permissions temporarily so git can work
sudo ~/.dotfiles/soften.sh ~/.dotfiles;

# Stash local edits, pull changes, and re-apply local edits
pushd ~/.dotfiles;
git stash;
git pull;
git stash apply;
popd;

# Permissions for files that should be owned by root
sudo ~/.dotfiles/harden.sh ~/.dotfiles;

# Synchronize system
~/.dotfiles/sync.sh;
