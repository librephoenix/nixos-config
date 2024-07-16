#!/bin/sh

# This will soften the security of these dotfiles, allowing
# the default unpriveleged user with UID/GID of 1000 to edit ALL FILES
# in the dotfiles directory

# This mainly is just here to be used by some scripts

# Run this inside of ~/.dotfiles (or whatever directory you installed
# the dotfiles to)

# Run this as root!

# BTW, this assumes your user account has a UID/GID of 1000

# After running this, YOUR UNPRIVELEGED USER CAN MAKE EDITS TO
# IMPORTANT SYSTEM FILES WHICH MAY COMPROMISE THE SYSTEM AFTER
# RUNNING nixos-rebuild switch!

if [ "$#" = 1 ]; then
    SCRIPT_DIR=$1;
else
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
fi
pushd $SCRIPT_DIR &> /dev/null;
sudo chown -R 1000:users .;
popd &> /dev/null;
