#!/bin/sh

# Automated script to update my non-primary systems
# config to be in sync with upstream git repo while
# preserving local edits to dotfiles via git stash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Relax permissions temporarily so git can work
sudo $SCRIPT_DIR/soften.sh $SCRIPT_DIR;

# Stash local edits, pull changes, and re-apply local edits
pushd $SCRIPT_DIR &> /dev/null;
git stash;
git pull;
git stash apply;
popd &> /dev/null;

# Permissions for files that should be owned by root
sudo $SCRIPT_DIR/harden.sh $SCRIPT_DIR;
