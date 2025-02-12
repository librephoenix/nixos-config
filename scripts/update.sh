#!/bin/sh

# Script to update my flake without
# synchronizing configuration

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
OUTOFDATEFLAKEREFS=(hyprland/hyprcursor)

# Update flake
pushd $SCRIPT_DIR/.. &> /dev/null;
sudo nix flake update "$@";
if [ "$#" -eq 0 ]; then
  sudo nix flake update $OUTOFDATEFLAKEREFS;
fi
popd &> /dev/null;
