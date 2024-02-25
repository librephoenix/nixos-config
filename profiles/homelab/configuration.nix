{ lib, pkgs, systemSettings, userSettings, ... }:

{
  imports =
    [ ../../system/hardware-configuration.nix
      ../../system/hardware/time.nix # Network time sync
      ../../system/security/doas.nix
      ../../system/security/gpg.nix
      ../../system/security/sshd.nix
      ( import ../../system/app/docker.nix {storageDriver = "btrfs"; inherit userSettings pkgs lib;} )
    ];

  # Fix nix path
  nix.nixPath = [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
                  "nixos-config=$HOME/dotfiles/system/configuration.nix"
                  "/nix/var/nix/profiles/per-user/root/channels"
                ];

  # Ensure nix flakes are enabled
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # I'm sorry Stallman-taichou
  nixpkgs.config.allowUnfree = true;

  # Kernel modules
  boot.kernelModules = [ "i2c-dev" "i2c-piix4" ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";

  # Networking
  networking.hostName = systemSettings.hostname; # Define your hostname.
  networking.networkmanager.enable = true; # Use networkmanager

  # Timezone and locale
  time.timeZone = systemSettings.timezone; # time zone
  i18n.defaultLocale = systemSettings.locale;
  i18n.extraLocaleSettings = {
    LC_ADDRESS = systemSettings.locale;
    LC_IDENTIFICATION = systemSettings.locale;
    LC_MEASUREMENT = systemSettings.locale;
    LC_MONETARY = systemSettings.locale;
    LC_NAME = systemSettings.locale;
    LC_NUMERIC = systemSettings.locale;
    LC_PAPER = systemSettings.locale;
    LC_TELEPHONE = systemSettings.locale;
    LC_TIME = systemSettings.locale;
  };

  # User account
  users.users.${userSettings.username} = {
    isNormalUser = true;
    description = userSettings.name;
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
    rclone
    rdiff-backup
    cryptsetup
    gocryptfs
    cryfs
  ];

  # I use zsh btw
  environment.shells = with pkgs; [ zsh ];
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

  # It is ok to leave this unchanged for compatibility purposes
  system.stateVersion = "22.11";

}
