#!/bin/sh

# Color codes for formatted output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No color

# Default parameters
SCRIPT_DIR="${HOME}/.dotfiles"
USER_EMAIL=""
SKIP_REVIEW=0
DISABLE_HARDEN=0
TEMP_CLONE=0
SKIP_CLONE=0
SKIP_EMAIL=0
EDITOR="${EDITOR:-nano}"  # Default to nano if EDITOR is not set

# Helper function to display usage message
show_help() {
  printf "${CYAN}Usage:${NC} $0 [OPTIONS]\n\n"
  printf "Options:\n"
  printf "  -d, --directory <path>     Specify the directory to clone the dotfiles (default: ~/.dotfiles)\n"
  printf "  -e, --email <email>        Provide an email to use for configuration (default: empty)\n"
  printf "  -y, --yes                  Skip editor confirmation for flake.nix review\n"
  printf "  -n, --no-harden            Skip the security hardening step\n"
  printf "  -t, --temp-clone           Clone into a temporary directory\n"
  printf "  -s, --skip-clone           Skip the cloning step if directory exists\n"
  printf "  -se, --skip-email          Skip the email replacement step in flake.nix\n"
  printf "  -h, --help                 Show this help message\n"
  exit 0
}

# Check dependencies
command -v nix-shell >/dev/null 2>&1 || { printf "${RED}Error: nix-shell is not installed.${NC}\n"; exit 1; }
command -v nixos-rebuild >/dev/null 2>&1 || { printf "${RED}Error: nixos-rebuild is not installed.${NC}\n"; exit 1; }
command -v nix >/dev/null 2>&1 || { printf "${RED}Error: nix is not installed.${NC}\n"; exit 1; }

# Parse arguments
while [ "$#" -gt 0 ]; do
  case "$1" in
    -d|--directory) SCRIPT_DIR="$2"; shift 2;;
    -e|--email) USER_EMAIL="$2"; shift 2;;
    -y|--yes) SKIP_REVIEW=1; shift;;
    -n|--no-harden) DISABLE_HARDEN=1; shift;;
    -t|--temp-clone) TEMP_CLONE=1; shift;;
    -s|--skip-clone) SKIP_CLONE=1; shift;;
    -se|--skip-email) SKIP_EMAIL=1; shift;;
    -h|--help) show_help;;
    --) shift; break;;
    *) printf "${RED}Error:${NC} Unknown option: $1\n"; show_help; exit 1;;
  esac
done

# Clone dotfiles repository, if not skipped
if [ "$SKIP_CLONE" -eq 0 ]; then
  if [ "$TEMP_CLONE" -eq 1 ]; then
    SCRIPT_DIR=$(mktemp -d)
    printf "${YELLOW}Cloning dotfiles to temporary directory ${SCRIPT_DIR}...${NC}\n"
  else
    printf "${CYAN}Cloning dotfiles to ${SCRIPT_DIR}...${NC}\n"
  fi
  nix-shell -p git --command "git clone https://gitlab.com/librephoenix/nixos-config $SCRIPT_DIR" || { printf "${RED}Failed to clone repository.${NC}\n"; exit 1; }
else
  if [ ! -d "$SCRIPT_DIR" ]; then
    printf "${RED}Error: Specified directory $SCRIPT_DIR does not exist. Cannot proceed without cloning.${NC}\n"
    exit 1
  fi
  printf "${YELLOW}Skipping clone step as requested; using existing directory $SCRIPT_DIR.${NC}\n"
fi

# Generate hardware configuration
printf "${CYAN}Generating hardware configuration...${NC}\n"
sudo nixos-generate-config --show-hardware-config > "$SCRIPT_DIR/system/hardware-configuration.nix" || { printf "${RED}Failed to generate hardware configuration.${NC}\n"; exit 1; }

# Determine boot mode (UEFI or BIOS) and set flake.nix accordingly
if [ -d /sys/firmware/efi/efivars ]; then
  printf "${GREEN}Detected UEFI boot mode.${NC}\n"
  sed -i "0,/bootMode.*=.*\".*\";/s//bootMode = \"uefi\";/" "$SCRIPT_DIR/flake.nix"
else
  printf "${GREEN}Detected BIOS boot mode.${NC}\n"
  sed -i "0,/bootMode.*=.*\".*\";/s//bootMode = \"bios\";/" "$SCRIPT_DIR/flake.nix"
  grubDevice=$(findmnt / | awk '{ print $2 }' | sed 's/\[.*\]//g' | tail -n 1 | lsblk -no pkname | tail -n 1)
  sed -i "0,/grubDevice.*=.*\".*\";/s//grubDevice = \"\/dev\/$grubDevice\";/" "$SCRIPT_DIR/flake.nix"
fi

# Customize flake.nix with user information
printf "${CYAN}Setting user-specific information in flake.nix...${NC}\n"
sed -i "0,/emmet/s//$(whoami)/" "$SCRIPT_DIR/flake.nix"
sed -i "0,/Emmet/s//$(getent passwd $(whoami) | cut -d ':' -f 5 | cut -d ',' -f 1)/" "$SCRIPT_DIR/flake.nix"
if [ "$SKIP_EMAIL" -eq 0 ]; then
  if [ -n "$USER_EMAIL" ]; then
    sed -i "s/emmet@librephoenix.com/$USER_EMAIL/" "$SCRIPT_DIR/flake.nix"
  else
    sed -i "s/emmet@librephoenix.com//" "$SCRIPT_DIR/flake.nix"
  fi
else
  printf "${YELLOW}Skipping email replacement in flake.nix as requested.${NC}\n"
fi
sed -i "s+~/.dotfiles+$SCRIPT_DIR+g" "$SCRIPT_DIR/flake.nix"

# Optional review of flake.nix
if [ "$SKIP_REVIEW" -eq 0 ]; then
  printf "${YELLOW}Opening flake.nix for manual review with ${EDITOR}...${NC}\n"
  $EDITOR "$SCRIPT_DIR/flake.nix"
fi

# Apply security hardening if enabled
if [ "$DISABLE_HARDEN" -eq 0 ]; then
  printf "${CYAN}Applying security hardening...${NC}\n"
  sudo "$SCRIPT_DIR/harden.sh" "$SCRIPT_DIR" || { printf "${RED}Hardening failed.${NC}\n"; exit 1; }
else
  printf "${YELLOW}Skipping security hardening as requested.${NC}\n"
fi

# Confirmation prompt for system rebuild
printf "${YELLOW}Ready to rebuild the system with nixos-rebuild. Do you want to proceed? (y/n) ${NC}"
read -r confirm
if [ "$confirm" != "y" ]; then
  printf "${RED}Aborting system rebuild.${NC}\n"
  exit 0
fi

# Rebuild system with new configuration
printf "${CYAN}Rebuilding system with nixos-rebuild...${NC}\n"
sudo nixos-rebuild switch --flake "$SCRIPT_DIR#system" || { printf "${RED}System rebuild failed.${NC}\n"; exit 1; }

# Build and switch to the user's home-manager configuration
printf "${CYAN}Setting up home-manager configuration...${NC}\n"
nix run home-manager/master --extra-experimental-features nix-command --extra-experimental-features flakes -- switch --flake "$SCRIPT_DIR#user" || { printf "${RED}Home-manager setup failed.${NC}\n"; exit 1; }

printf "${GREEN}Installation and configuration complete!${NC}\n"
