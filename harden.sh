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
chown -R root:root system;
chown -R root:root patches;
chown root:root flake.lock;
chown root:root flake.nix
chown root:root profiles/*/configuration.nix;
chown 1000:users **/README.org;
chown root:root harden.sh;
popd &> /dev/null;
