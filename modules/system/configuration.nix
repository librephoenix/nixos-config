{ config, lib, pkgs, ... }:

{
  config = {
    # Packages
    environment.systemPackages = with pkgs; [ git ];

    # Journal
    services.journald.extraConfig = "SystemMaxUse=50M\nSystemMaxFiles=5";
    services.journald.rateLimitBurst = 500;
    services.journald.rateLimitInterval = "30s";

    # Locale and TZ
    time.timeZone = "America/Chicago";
    services.timesyncd.enable = true;
    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ADDRESS = config.i18n.defaultLocale;
      LC_IDENTIFICATION = config.i18n.defaultLocale;
      LC_MEASUREMENT = config.i18n.defaultLocale;
      LC_MONETARY = config.i18n.defaultLocale;
      LC_NAME = config.i18n.defaultLocale;
      LC_NUMERIC = config.i18n.defaultLocale;
      LC_PAPER = config.i18n.defaultLocale;
      LC_TELEPHONE = config.i18n.defaultLocale;
      LC_TIME = config.i18n.defaultLocale;
    };

    # Use zsh
    programs.zsh.enable = true;
    environment.shells = with pkgs; [ zsh ];
    users.defaultUserShell = pkgs.zsh;

    # Fix nix path
    nix.nixPath = [ "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
                    "nixos-config=$HOME/dotfiles/system/configuration.nix"
                    "/nix/var/nix/profiles/per-user/root/channels"
                  ];

    # Ensure nix flakes are enabled
    nix.extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # Substituters
    nix.settings = {
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    # wheel group gets trusted access to nix daemon
    nix.settings.trusted-users = [ "@wheel" ];

    # Bootloader
    # Use systemd-boot if uefi, default to grub otherwise
    boot.loader.systemd-boot.enable = true;
    boot.loader.systemd-boot.editor = false;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot";

    # Silent Boot
    # https://wiki.archlinux.org/title/Silent_boot
    boot.kernelParams = [
      "quiet"
      "splash"
      "vga=current"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "udev.log_priority=3"
    ];
    boot.initrd.systemd.enable = true;
    boot.initrd.verbose = false;
    boot.plymouth.enable = true;

    # Networking
    networking.networkmanager.enable = true; # Use networkmanager

    # Remove bloat
    programs.nano.enable = lib.mkForce false;

    # Localsend is helpful for setting up new systems or quickly transferring files
    programs.localsend.enable = true;
    programs.localsend.openFirewall = true;
  };

}
