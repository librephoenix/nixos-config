#!/bin/sh

# Script to update my flake without
# synchronizing configuration

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Update flake
pushd $SCRIPT_DIR/.. &> /dev/null;
sudo nix flake update "$@";
popd &> /dev/null;
