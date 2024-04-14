#!/bin/sh

# Script to synchronize system state
# with configuration files for nixos system
# and home-manager

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Install and build home-manager configuration
home-manager switch --flake $SCRIPT_DIR#user;

$SCRIPT_DIR/sync-posthook.sh
