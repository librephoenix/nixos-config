#!/bin/sh

# Script to synchronize system state
# with configuration files for nixos system
# and home-manager

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

$SCRIPT_DIR/sync-system.sh
$SCRIPT_DIR/sync-user.sh
