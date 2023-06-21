{ config, lib, pkgs, blocklist-hosts, myName, myHostname, myTimezone, myLocale, myTheme, myBackgroundUrl, myBackgroundSha256, ... }:

args@{ ... }:

{
  imports =
    [ ../../system/hardware-configuration.nix
      ../../system/security/doas.nix
      ../../system/security/gpg.nix
      ../../system/security/sshd.nix
      ( import ../../system/app/docker.nix ( args {storageDriver = "btrfs";} ) )
    ];

  # Fix nix path
  nix.nixPath = [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
                  "nixos-config=$HOME/dotfiles/system/configuration.nix"
                  "/nix/var/nix/profiles/per-user/root/channels"
                ];

  # Experimental features
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # I'm sorry Stallman-taichou
  nixpkgs.config.allowUnfree = true;

  # Kernel modules
  boot.kernelModules = [ "i2c-dev" "i2c-piix4" ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Networking
  networking.hostName = myHostname; # Define your hostname.
  networking.networkmanager.enable = true; # Use networkmanager

  # Timezone and locale
  time.timeZone = myTimezone; # time zone
  i18n.defaultLocale = myLocale;
  i18n.extraLocaleSettings = {
    LC_ADDRESS = myLocale;
    LC_IDENTIFICATION = myLocale;
    LC_MEASUREMENT = myLocale;
    LC_MONETARY = myLocale;
    LC_NAME = myLocale;
    LC_NUMERIC = myLocale;
    LC_PAPER = myLocale;
    LC_TELEPHONE = myLocale;
    LC_TIME = myLocale;
  };

  # User account
  users.users.${myName} = {
    isNormalUser = true;
    description = "Emmet";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
    uid = 1000;
  };

  # System packages
  environment.systemPackages = with pkgs; [
    vim
    wget
    zsh
    git
  ];

  # I use zsh btw
  environment.shells = with pkgs; [ zsh ];
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

  # It is ok to leave this unchanged for compatibility purposes
  system.stateVersion = "22.11";

}
