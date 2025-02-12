#!/bin/sh

# Script to build all systems
# And push to attic cache

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )


pushd $SCRIPT_DIR/.. &> /dev/null;
nixos-rebuild build --flake .#snowfire;
attic push emmet ./result;
rm ./result;
nixos-rebuild build --flake .#polarias;
attic push emmet ./result;
rm ./result;
nixos-rebuild build --flake .#zenith;
attic push emmet ./result;
rm ./result;
nixos-rebuild build --flake .#stardust;
attic push emmet ./result;
rm ./result;
nixos-rebuild build --flake .#ori;
attic push emmet ./result;
rm ./result;
