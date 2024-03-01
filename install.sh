#!/bin/sh

# Automated script to install my dotfiles

# Clone dotfiles
nix-shell -p git --command "git clone https://gitlab.com/librephoenix/nixos-config ~/.dotfiles"

# Generate hardware config for new system
sudo nixos-generate-config --show-hardware-config > ~/.dotfiles/system/hardware-configuration.nix

# Check if uefi or bios
if [ -d /sys/firmware/efi/efivars ]; then
    sed -i "0,/bootMode.*=.*\".*\";/s//bootMode = \"uefi\";/" ~/.dotfiles/flake.nix
else
    sed -i "0,/bootMode.*=.*\".*\";/s//bootMode = \"bios\";/" ~/.dotfiles/flake.nix
    grubDevice=$(findmnt / | awk -F' ' '{ print $2 }' | sed 's/\[.*\]//g' | tail -n 1 | lsblk -no pkname | tail -n 1 )
    sed -i "0,/grubDevice.*=.*\".*\";/s//grubDevice = \"\/dev\/$grubDevice\";/" ~/.dotfiles/flake.nix
fi

# Patch flake.nix with different username/name and remove email by default
sed -i "0,/emmet/s//$(whoami)/" ~/.dotfiles/flake.nix
sed -i "0,/Emmet/s//$(getent passwd $(whoami) | cut -d ':' -f 5 | cut -d ',' -f 1)/" ~/.dotfiles/flake.nix
sed -i "s/emmet@librephoenix.com//" ~/.dotfiles/flake.nix

# Open up editor to manually edit flake.nix before install
if [ -z "$EDITOR" ]; then
    EDITOR=nano;
fi
$EDITOR ~/.dotfiles/flake.nix;

# Rebuild system
sudo nixos-rebuild switch --flake ~/.dotfiles#system;

# Install and build home-manager configuration
nix run home-manager/master --extra-experimental-features nix-command --extra-experimental-features flakes -- switch --flake ~/.dotfiles#user;

# Permissions for files that should be owned by root
sudo ~/.dotfiles/harden.sh ~/.dotfiles;
