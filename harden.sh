#!/bin/sh

# This will harden the security of these dotfiles, preventing
# unpriveleged users from editing system-level (root configuration)
# files maliciously

# Run this inside of ~/.dotfiles (or whatever directory you installed
# the dotfiles to)

# Run this as root!

# BTW, this assumes your user account has a PID/GID of 1000

# After running this, the command `nix flake update` will require root

if [ "$#" = 1 ]; then
    dotfilesDir=$1;
else
    dotfilesDir=$(pwd);
fi
pushd $dotfilesDir &> /dev/null;
chown 0:0 .;
chown 0:0 profiles/*;
chown -R 0:0 system;
chown -R 0:0 patches;
chown 0:0 flake.lock;
chown 0:0 flake.nix
chown 0:0 profiles/*/configuration.nix;
chown 0:0 harden.sh;
chown 1000:users **/README.org;
popd &> /dev/null;
